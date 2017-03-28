{- 
 - Copyright 2017 Daniel Eachern Huang
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -    http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Compile.Compile where

import Text.PrettyPrint
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Fresh
import AstUtil.Var
import Low.CgLowMM
import qualified Core.RwCore as RW
import Core.CoreSyn
import Core.CgConj
import qualified Core.RnCore as RnC
import Core.DensSyn
import Core.KernSyn
import qualified Low.LowShpSyn as S
import qualified Low.LowXXSyn as LX
import Core.CoreTySyn
-- import qualified CudaC.CodeGenC as CCg
import qualified CudaC.CgCuda as CuCg
import CudaC.CgStInit
import qualified CudaC.CudaCSyn as C
import Compile.CompData
import Core.LintCore
import Core.CgLike(runLLFn)
import Core.CgGradProp
import Core.CgDisc
import Core.CgESlice
import Core.CgDet
import Core.CgUserProp
import Core.DepCore
import qualified Rv.RvSyn as Rv
import Rv.LowerRv
import Xface.PyXface
import CudaC.CgKern
import CudaC.CgMcmcRepr
import Xface.PySyn as Py
    
    
----------------------------------------------------------------------
-- = Compile Description
{-| [Note]

========================================
Front-end (surface language to Core IL)

(Rv String, Maybe (KernU String))            (unsanitized)
              |
(Dens (TVar Typ), Maybe (KernU (TVar Typ))   (sanitized, linearized exp, types)


========================================
Middle-end (Core IL to high-level inference)

(Dens (TVar Typ), Maybe (KernU (TVar Typ)))
      |
Kern (Dens (TVar Typ))
          \
           \
      Dens (TVar Typ)         (linearized exp, types)
            |
      Low P.Prim (TVar Typ)   (linearized exp, types)


========================================
Back-end (High-level inference to native inference)

      Low P.Prim (TVar Typ)   (linearized exp, types)
                | (runCgLowMM)
      Low M.Prim (TVar Typ)   (linerarized exp, types, simpl proj, allocs)
                | (codeGenC)
      C (TVar C.Typ)          (linerarized exp, types, simpl proj)

-}


-----------------------------------
-- == Frontend

gatherCatKern :: (TypedVar b Typ) => KernU b -> Map.Map b Int
gatherCatKern (Base _ _ fn _ _ _) = gatherCat' fn
gatherCatKern (Tensor k1 k2) = gatherCatKern k1 `Map.union` gatherCatKern k2

                               
normFn :: Map.Map (TVar Typ) Int -> TVar Typ -> Fn (TVar Typ) -> Fn (TVar Typ)
normFn catCtx vMod fn =
    let fn' = prodFn (RW.unfactor (RW.factor fn))
        (hd, tl) = RW.split vMod fn'
        fn'' = RW.runMix catCtx hd (prodFn tl)
    in
      RW.factor (Prod hd fn'')

        
frontend :: CompInfo -> Rv.Model -> Maybe (KernU String) -> CompM (ModDecls (TVar Typ), Map.Map (TVar Typ) (TVar Typ), Fn (TVar Typ), Maybe (KernU (TVar Typ)) )
frontend cinfo model kern =
    do (modDecls, fn) <- lowerProg model           
       (varsM, modDecls') <- lift $ RnC.runRnModDeclsTyVar cinfo (Rv.model_params model ++ modDecls)
       fn' <- RnC.runRnFnTyVar cinfo varsM modDecls' fn
       kern' <- rnKern varsM modDecls'
       fn'' <- runLintFn cinfo modDecls' fn'
       let fn''' = prodFn (RW.unfactor (RW.factor fn''))
       debugM "[Compile.Compile]" $ "After frontend normalization (Density):\n" ++ pprShow fn'''
       -- debugM "Compile" ("[Compile] @frontend | Kernel:\n" ++ pprShow kern')
       propVarsM <- mkPropVarsM modDecls'
       return (modDecls', propVarsM, fn''', kern')
    where
      rnKern varsM modDecls =
          case kern of
            Just kern' ->
                do kern'' <- RnC.runRnKernUTyVar cinfo varsM modDecls kern' >>= return . Just
                   return kern''
            Nothing -> return Nothing

      mkPropVarsM :: ModDecls (TVar Typ) -> CompM (Map.Map (TVar Typ) (TVar Typ))
      mkPropVarsM modDecls =
          mapM (\v -> do v' <- lift $ mkTyIdIO (getGenSym cinfo) (varName v) ModParamDup (getType' v)
                         return (v, v')
               ) (getModParamIds modDecls) >>= return . Map.fromList 
          
          
kernHeuristic1 :: Fn (TVar Typ) -> [TVar Typ] -> CompM (KernU (TVar Typ))
kernHeuristic1 fn vs_mod =
    do kerns <- mapM heuristic vs_mod
       return $ go kerns
    where
      normFn' v = normFn (gatherCat' fn) v (RW.fullConds' [v] fn)
      
      heuristic v =
          do let 
                 (p_fn, l_fns) = RW.split v (normFn' v)
                 l_fn = prodFn l_fns
             traceM $ "CHECKING " ++ pprShow p_fn ++ " with " ++ pprShow l_fn
             b <- chkConjM p_fn l_fn
             if b             
             then return $ mkUserConjGibbs v
             else
                 if isDiscFn p_fn
                 then return $ mkUserDiscGibbs v
                 else return $ mkUserHMC [v] Nothing

      go [] = error $ "[Compile.Compile] @kernify | Empty kernel"
      go (kern:[]) = kern
      go (kern:kerns) = Tensor kern (go kerns)
          
          
kernify :: ModDecls (TVar Typ) -> Fn (TVar Typ) -> Maybe (KernU (TVar Typ)) -> CompM (KernU (TVar Typ))
kernify modDecls fn kern =
    case kern of
      Just kern' -> go kern'
      Nothing ->
          do let modParams = getModParamIds modDecls
             if length modParams == 0
             then throwError $ "No model parameters to fit"                  
             else do kern' <- kernHeuristic1 fn modParams
                     go kern'
    where
      go (Base kind ku _ allocs kernParams code) =
          case ku of
            Single vMod -> 
                return $ Base kind ku (normFn (gatherCat' fn) vMod (RW.fullConds' (kuVars ku) fn)) allocs kernParams code
            Block _ ->
                return $ Base kind ku (RW.fullConds' (kuVars ku) fn) allocs kernParams code
      go (Tensor k1 k2) =
          do k1' <- go k1
             k2' <- go k2
             return $ Tensor k1' k2'
                       

-----------------------------------
-- == Middle-end

type CgMidM = StateT (InferCtx (TVar Typ)) CompM
    
    
mkBlkId :: CompInfo -> [TVar Typ] -> CgMidM (TVar Typ, TVar Typ)
mkBlkId cinfo v_blks =
    do v_currBlk <- lift $ lift $ mkTyIdIO (getGenSym cinfo) (blkName v_blks) (ModParam PK_Prob) (BlkTy [])
       v_propBlk <- lift $ lift $ mkTyIdIO (getGenSym cinfo) (blkName v_blks) ModParamDup (BlkTy [])
       modify (\st -> st { ic_dupCtx = Map.insert v_currBlk v_propBlk (ic_dupCtx st) })
       modify (\st -> st { ic_modBlkCtx = Map.insert v_currBlk v_blks (ic_modBlkCtx st) })
       modify (\st -> st { ic_modDecls = ic_modDecls st ++ [ (ModParam PK_Prob, v_currBlk, BlkTy []) ]})
       return (v_currBlk, v_propBlk)
              
              
cgMidend' :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> KernU (TVar Typ) -> CgMidM (Kern (LX.LowPP (TVar Typ)) (TVar Typ))
cgMidend' cinfo copt inferCtx k =
    case k of
      Base kind ku fn _ kernParams code ->
          case kind of            
            Gibbs (Conj _ _) ->
                do let vMod = head (kuVars ku)
                   kern <- lift $ runConjFn cinfo copt inferCtx vMod fn
                   return kern
            Gibbs (Disc _) ->                
                do kern <- lift $ runDiscFn cinfo copt inferCtx (head (kuVars ku)) fn
                   return kern
            GradProp (HMC _ _ simLen stepSize) ->
                do let v_wrt = kuVars ku
                   (v_currBlk, v_propBlk) <- mkBlkId cinfo v_wrt
                   kern <- lift $ runHmcFn cinfo copt inferCtx v_wrt v_currBlk v_propBlk simLen stepSize fn
                   return kern
            Slice (Ellip _ _) -> 
                do let v_mod = head (kuVars ku)
                   kern <- lift $ runESliceFn cinfo copt inferCtx v_mod fn
                   return kern
            UserProp (MWG _ _ _ _) ->                
                do let v_mod = head (kuVars ku)
                       (fn', _) = RW.split v_mod fn
                   propFn <- lift $ runLowerUserCode fn' code           
                   kern <- lift $ runMwgKern cinfo copt inferCtx v_mod fn propFn
                   return kern
            _ -> error $ "[Compile] @ cgMidend | TODO " ++ pprShow kind
      Tensor k1 k2 ->
          do k1' <- cgMidend' cinfo copt inferCtx k1
             k2' <- cgMidend' cinfo copt inferCtx k2
             return $ Tensor k1' k2'

                    
midendFn :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> KernU (TVar Typ) -> CompM (Kern (LX.LowPP (TVar Typ)) (TVar Typ), InferCtx (TVar Typ))
midendFn cinfo copt inferCtx k =
    do k' <- runLintKernU cinfo (ic_modDecls inferCtx) k       
       (k'', inferCtx') <- runStateT (cgMidend' cinfo copt inferCtx k') inferCtx
       return (k'', inferCtx')

              
-----------------------------------
-- == Backend

backendDecl :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> TVar C.Typ -> LX.LowPP (TVar Typ) -> CompM (S.ShpCtx (TVar Typ), [C.Decl (TVar C.Typ)])
backendDecl cinfo copt inferCtx v_rng lowppDecl =
    do lowmmDecl <- runCgLowMM cinfo copt inferCtx lowppDecl
       {-
       cDecls <- CCg.runCgDecl cinfo inferCtx v_rng lowmmDecl
       return (LX.getGlobs (LX.unLowMM lowmmDecl), cDecls)
       -}
       let rtSizes = Map.empty -- TODO: HACK
       (shpCtx', cDecls) <- CuCg.runCgDecl cinfo copt inferCtx (getTarget copt) rtSizes v_rng lowmmDecl
       return (shpCtx', cDecls)
       
       
              
              
backendDecls :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> TVar C.Typ -> [LX.LowPP (TVar Typ)]  -> CompM (S.ShpCtx (TVar Typ), [C.Decl (TVar C.Typ)])
backendDecls cinfo copt inferCtx v_rng decls =
    do (shpCtxs, decls') <- mapM (backendDecl cinfo copt inferCtx v_rng) decls >>= return . unzip
       let cdecls = concat decls'
           shpCtx = foldl (\acc shpCtx' -> acc `Map.union` shpCtx') Map.empty shpCtxs
       return (shpCtx, cdecls)


runBackendDecls :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> TVar C.Typ -> [LX.LowPP (TVar Typ)] -> IO (Either String (S.ShpCtx (TVar Typ), [C.Decl (TVar C.Typ)]))
runBackendDecls cinfo copt inferCtx v_rng decls =
    runExceptT (backendDecls cinfo copt inferCtx v_rng decls)


-----------------------------------
-- == Compile


cgDet :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> [(TVar Typ, Fn (TVar Typ))] -> CompM (Map.Map (TVar Typ) (LX.LowPP (TVar Typ)))
cgDet cinfo copt inferCtx detFns =
    do detCtx <- mapM (\(v_mod, fn) ->
                           do det <- runDetFn' cinfo copt inferCtx v_mod fn
                              return (v_mod, det)
                      ) detFns
       return $ Map.fromList detCtx
                        

cgLLMod :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> TVar C.Typ -> Fn (TVar Typ) -> CompM (S.ShpCtx (TVar Typ), [C.Decl (TVar C.Typ)])
cgLLMod cinfo copt inferCtx v_rng fn =
    do llAll <- runLLFn cinfo copt inferCtx (mkName "ll_mod") fn
       llCall <- runCgCallLLMod
       let foo = LX.LowPP (LX.LowXX Map.empty False (LX.HostCall False) [] llAll)
       (shpCtx, llAll') <- backendDecl cinfo copt inferCtx v_rng foo
       return (shpCtx, llAll' ++ [ llCall ])                   

              
compile :: Rv.Model -> Maybe (KernU String) -> Target -> CompM (Py.Decl (TVar Typ), String, String, C.Prog (TVar C.Typ))
compile model infer target =
    do -- handle <- initCompLogger
       genSym <- lift $ newGenSym
       let cinfo = CompInfo genSym
           copt = CompOpt True target
       (modDecls, dupCtx, fn, kern) <- frontend cinfo model infer
       let tlDepG = depTopLevel fn
       debugM "[Compile.Compile]" $ " | Top-level dependencies: " ++ pprShow tlDepG
       let pyTys = genPyTyp modDecls
           ord = getModDeclIds modDecls
           fns = map (\fn' -> (densPtVar (gatherDensPt fn'), fn')) (RW.unfactorInOrd ord fn)
       v_idxs <- lift $ mkTyIdIO (getGenSym cinfo) (mkName "idxs") ModAux (VecTy IntTy)
       let inferCtx = IC Map.empty dupCtx modDecls v_idxs
       pyModParam <- runCgPyModParams cinfo modDecls fns -- HERE
       debugM "[Compile.Compile]" $ "Python model parameters: " ++ pprShow pyModParam
       kern' <- kernify modDecls fn kern
       (kern'', inferCtx') <- midendFn cinfo copt inferCtx kern'
       detCtx <- cgDet cinfo copt inferCtx (filter (\(_, fn') -> isDirac fn') fns)
       v_rng <- lift $ mkTyIdIO genSym (mkName "rng") ModAux (C.PtrTy (C.NameTy "augur_rng"))
       -- v_idxs <- lift $ mkTyIdIO genSym (mkName "idxs") ModAux (C.VecTy C.IntTy)
       (shpCtx, cdecls) <- backendDecls cinfo copt inferCtx' v_rng (gatherCode kern'' ++ Map.elems detCtx)
       (shpCtx', llMod) <- cgLLMod cinfo copt inferCtx' v_rng fn 
       let shpCtx'' = shpCtx `Map.union` shpCtx'                      
       mcmcInit <- runCgStInit cinfo (getTarget copt) v_rng inferCtx' shpCtx'' kern''
                   
       chdr <- runCgHdr cinfo inferCtx' kern'' shpCtx'' v_rng
       mcmcStep <- runCgMcmcDecl cinfo target tlDepG (mapCode (LX.getDecl . LX.unLowPP) kern'')
       mcmcSetPt <- runCgStSetPt cinfo (getTarget copt) inferCtx'
       mcmcCpy <- runCgStCpy cinfo (getTarget copt) inferCtx'
       
       -- closeCompLogger handle 
       return (pyModParam, pyTys, chdr, C.Prog (cdecls ++ llMod ++ [ mcmcStep, mcmcSetPt, mcmcCpy, mcmcInit ]))
           
