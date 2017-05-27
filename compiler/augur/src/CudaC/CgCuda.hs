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

{-# LANGUAGE BangPatterns #-}

module CudaC.CgCuda
    ( runCgDecl
    , runCgDecls ) where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Traversable as T
import qualified Data.Map as Map
import Data.IORef
import Debug.Trace

import AstUtil.Pretty
import AstUtil.Fresh
import AstUtil.Var
import Comm.DistSyn
import Core.CoreSyn
import Compile.CompData
import CudaC.XfaceCudaC hiding (compErr)
import qualified CudaC.CudaCSyn as C
import Low.LowSyn
import qualified Low.LowXXSyn as LX
import Core.CoreTySyn
import Low.LowpPrimSyn
import CudaC.CgCudaCCore hiding (compErr)
import CudaC.SplitBlk
import qualified CudaC.BlkSyn as B
import qualified CudaC.PllBlk as PB
import qualified Low.LowShpSyn as S
import Low.InlineLow

----------------------------------------------------------------------
-- = CgCuda Description
{-| [Note]

This module contains code-generation to C/Cuda code.

-}

{-| [Note]

grad_foobar(x1, ..., xn) {
  adj_a += ...
  for (i <- gen) {
    ...
    adj_b[x] += ...
  }
}

->

foobar(x1, ..., xn) {
  
}

-}


compErr :: String -> a
compErr msg = compErrMod "CgCuda" msg

-----------------------------------
-- == Types and operations

type CgM = RWST CgRdr [()] CgSt CompM
data CgRdr =
    CR { cr_vRng :: TVar C.Typ
       , cr_vIdx :: TVar Typ
       , cr_vAux :: TVar C.Typ
       , cr_vCurr :: TVar C.Typ
       , cr_vProp :: TVar C.Typ
       , cr_pllCtx :: Maybe (TVar Typ, Gen (TVar Typ))  -- Parallelization index
       , cr_target :: C.Exp (TVar C.Typ)
       , cr_genSym :: GenSym }
data CgSt =
    CS { -- when parallelizing, may need to allocate more memory
         cs_shpCtxUp :: Map.Map (TVar Typ) Bool
       , cs_shpCtx :: Map.Map (TVar Typ) (Typ, S.Shp (TVar Typ))
         -- cs_shpCtx :: S.ShpCtx (TVar Typ)
       }

    
freshId :: Name -> IdKind -> t -> CgM (TVar t)
freshId name ik ty =
    do genSym <- asks cr_genSym
       lift $ lift $ mkTyIdIO genSym name ik ty 


withPllCtx :: (TVar Typ, Gen (TVar Typ)) -> CgM a -> CgM a
withPllCtx pllCtx =
    local (\rdr -> rdr { cr_pllCtx = Just pllCtx })


          
-----------------------------------
-- == Transformations              

                       
cgRng :: [C.Exp (TVar C.Typ)] -> CgM [C.Exp (TVar C.Typ)]
cgRng es =
    do v_rng <- asks cr_vRng
       pllCtx <- asks cr_pllCtx
       case pllCtx of
         Just (idx, _) -> return $ (C.Var v_rng + C.Var (cgIdLhs idx)) : es
         Nothing -> return $ C.Var v_rng : es

              
cgExpDet :: Exp (TVar Typ) -> C.Exp (TVar C.Typ)
cgExpDet (Var x) = cgIdRhs x
cgExpDet (Lit lit) = cgLit lit
cgExpDet (Proj e es) = cgProj (tyOf e) (cgExpDet e) (map cgExpDet es)
cgExpDet e = error $ "@CgCuda | Shouldn't happen " ++ pprShow e

              
cgExp :: Exp (TVar Typ) -> CgM (C.Exp (TVar C.Typ))
cgExp (Var x) = return $ cgIdRhs x
cgExp (Lit lit) = return $ cgLit lit
cgExp (DistOp dop _ dist es) =
    do es' <- mapM cgExp es
       let fn = getDistLibFn dop dist
       case dop of
         Conj _ _ ->
             do es'' <- cgRng es'
                return $ C.LibCall fn es''
         Sample ->
             do es'' <- cgRng es'
                return $ C.LibCall fn es''
         _ -> return $ C.LibCall fn es'
cgExp (Call ce es) =    
    do es' <- mapM cgExp es
       case ce of
         FnId _ -> compErr $ "Not supported yet, need to extend with model state"
         PrimId _ pm prim ->
             case prim of
               NormAndSamp ->
                   do es'' <- cgRng es'
                      return $ cgPrim prim pm es''
               _ -> return $ cgPrim prim pm es'
cgExp (Proj e es) =
    do e' <- cgExp e
       es' <- mapM cgExp es
       return $ cgProj (tyOf e) e' es'      

              
cgGen :: TVar Typ -> Gen (TVar Typ) -> CgM (C.Exp (TVar C.Typ), C.Exp (TVar C.Typ), C.Exp (TVar C.Typ))
cgGen x (Until e1 e2) =
    do let x' = cgIdLhs x
       e1' <- cgExp e1
       e2' <- cgExp e2
       return ( C.Assign (C.Var x') C.EqAss e1'
              , C.Binop (C.Var x') C.Lt e2'
              , C.Assign (C.Var x') C.PlusEqAss 1 )


cgSetIdx :: [Exp (TVar Typ)] -> CgM (C.Stmt (TVar C.Typ))
cgSetIdx idxs =
    do v_idx <- asks cr_vIdx
       cgStmt (seqStmt (map (\(idx, i) -> Store v_idx [Lit (Int i)] Update idx) (zip idxs [0..])))


cgProjIdx :: [TVar Typ] -> CgM (C.Stmt (TVar C.Typ))
cgProjIdx idxs = 
    do v_idx <- asks cr_vIdx
       cgStmt (seqStmt (map (\(idx, i) -> Assign idx (Proj (Var v_idx) [Lit (Int i)])) (zip idxs [0..])))


cgMcmcCall :: Exp (TVar Typ) -> CgM (C.Stmt (TVar C.Typ))
cgMcmcCall (Call (PrimId _ _ (MWG prop swap mwg)) es) =
    do vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       s_idxs <- cgSetIdx (drop 3 es)
       let v_ll = case es !! 2 of
                    Var x -> cgIdLhs x
                    _ -> compErr $ "HAZORZ alert"
           es'' = [ C.Var vAux, C.Var vCurr, C.Var vProp, C.Var v_ll
                  , C.mkLiterally (nameToStr prop)
                  , C.mkLiterally (nameToStr swap)
                  , C.mkLiterally (nameToStr mwg) ]
           s_call = C.mkLibCall "augur_mcmc_mwg" es''
       return $ C.seqStmt [ s_idxs, s_call ]
cgMcmcCall (Call (PrimId _ _ (EllipSlice likeOne)) es) =
    do es' <- mapM cgExp es
       vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       s_idxs <- cgSetIdx (drop 6 es)
       let es'' = [ C.Var vAux, C.Var vCurr, C.Var vProp ] ++ 
                  (take 6 es') ++ [ C.mkLiterally (nameToStr likeOne) ]
           s_call = C.mkLibCall "augur_mcmc_eslice" es''
       return $ C.seqStmt [ s_idxs, s_call ]
cgMcmcCall (Call (PrimId _ _ (LeapFrog grad prop)) es) =
    do es' <- mapM cgExp es
       vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       target <- asks cr_target
       let es'' = [ target, C.Var vAux, C.Var vCurr, C.Var vProp ] ++ es' ++ 
                  [ C.mkLiterally (nameToStr grad), C.mkLiterally (nameToStr prop) ]
           s_call = C.mkLibCall "h_augur_mcmc_hmc" es''
       return s_call
cgMcmcCall (Call (PrimId _ _ (ReflSlice grad prop)) es) =
    do es' <- mapM cgExp es
       vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       target <- asks cr_target
       let es'' = [ target, C.Var vAux, C.Var vCurr, C.Var vProp ] ++ es' ++ 
                  [ C.mkLiterally (nameToStr grad), C.mkLiterally (nameToStr prop) ]
           s_call = C.mkLibCall "h_augur_mcmc_refl_slice" es''
       return s_call
cgMcmcCall (Call (PrimId _ _ (NUTS grad prop)) es) =
    do es' <- mapM cgExp es
       vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       target <- asks cr_target
       let es'' = [ target, C.Var vAux, C.Var vCurr, C.Var vProp ] ++ es' ++ 
                  [ C.mkLiterally (nameToStr grad), C.mkLiterally (nameToStr prop) ]
           s_call = C.mkLibCall "h_augur_mcmc_nuts" es''
       return s_call
cgMcmcCall _ =
    return $ C.mkSkip

           
-- TODO: MOVEME         
genToShpExp :: (BasicVar b) => Gen b -> S.ShpExp b
genToShpExp (Until e1 e2) =
    case e2 of
      Var x -> S.Val (S.Var x)
      Lit (Int i) -> S.Val (S.Int i)
      _ -> error $ "[CgCuda] | Unexpected generator expressions " ++ pprShow (Until e1 e2)


updateShp :: TVar Typ -> Gen (TVar Typ) -> CgM ()
updateShp v gen =
    modify (\st -> case (Map.lookup v (cs_shpCtxUp st), Map.lookup v (cs_shpCtx st)) of
                     (Just False, Just (_, shp)) ->
                         let ty' = VecTy (getType' v)
                             v' = setType v ty'
                             shp' = S.SingConn (genToShpExp gen) shp
                         in 
                           -- Kind of weird ... 
                           st { cs_shpCtxUp = Map.insert v' True (cs_shpCtxUp st)
                              , cs_shpCtx = Map.insert v' (ty', shp') (cs_shpCtx st) }
                     (Just True, _) -> st
                     _ -> error $ "Lookup of " ++ pprShow v ++ " failed in shape context: " ++ pprShow (cs_shpCtx st)
           )


upVarTy :: TVar Typ -> CgM (TVar Typ)
upVarTy v =
    do shpCtx <- gets cs_shpCtx
       case Map.lookup v shpCtx of
         Just (ty, _) -> return $ setType v ty
         Nothing -> return v

                    
upVarTys :: [TVar Typ] -> CgM [TVar Typ]
upVarTys = mapM upVarTy

                    
cgReadVecFromShape :: TVar Typ -> [Exp (TVar Typ)] -> CgM (C.Stmt (TVar C.Typ))
cgReadVecFromShape x es =
    do v_view <- freshId Anon Local ty_AugurVec 
       pllCtx <- asks cr_pllCtx
       es' <- mapM cgExp es
       (libFn, args) <- select es' pllCtx
       let s1 = C.assignStmt v_view (C.LibCall libFn args)
           s2 = C.assignStmt' (C.Var (cgIdLhs x)) (C.addrOf (C.Var v_view))
       return $ C.seqStmt [ s1, s2 ]
    where
      select args pllCtx =
          case pllCtx of
            Just (idx, gen) ->
                do let v = atmExpVar (es !! 0)
                   updateShp v gen
                   return ("augur_vec_pll_view_as", args ++ [ cgIdRhs idx ])
            Nothing ->
                return ("augur_vec_view_as", args)
                       

cgReadMatFromShape :: TVar Typ -> [Exp (TVar Typ)] -> CgM (C.Stmt (TVar C.Typ))
cgReadMatFromShape x es =
    do v_view <- freshId Anon Local ty_AugurMat
       pllCtx <- asks cr_pllCtx
       es' <- mapM cgExp es
       (libFn, args) <- select es' pllCtx
       let s1 = C.assignStmt v_view (C.LibCall libFn args)
           s2 = C.assignStmt' (C.Var (cgIdLhs x)) (C.addrOf (C.Var v_view))
       return $ C.seqStmt [ s1, s2 ]
    where
      select args pllCtx =
          case pllCtx of
            Just (idx, gen) ->
                do let v = atmExpVar (es !! 0)
                   updateShp v gen
                   return ("augur_mat_pll_view_as", args ++ [ cgIdRhs idx ])
            Nothing ->
                return ("augur_mat_view_as", args)

                       
cgStmt :: Stmt (TVar Typ) -> CgM (C.Stmt (TVar C.Typ))
cgStmt Skip =
    return $ C.mkSkip
cgStmt (Exp e) =
    case e of
      Call (PrimId _ _ (MWG _ _ _)) _ -> cgMcmcCall e
      Call (PrimId _ _ (EllipSlice _)) _ -> cgMcmcCall e
      Call (PrimId _ _ (LeapFrog _ _)) _ -> cgMcmcCall e
      Call (PrimId _ _ (ReflSlice _ _)) _ -> cgMcmcCall e
      Call (PrimId _ _ (NUTS _ _)) _ -> cgMcmcCall e
      _ -> cgExp e >>= \e' -> return $ C.Exp e'
cgStmt (Assign x e) =
    case e of
      Call (PrimId _ _ ReadVecFromShape) es -> cgReadVecFromShape x es          
      Call (PrimId _ _ ReadMatFromShape) es -> cgReadMatFromShape x es          
      _ -> 
          do let x' = cgIdLhs x
             e' <- cgExp e
             return $ C.assignStmt x' e'
cgStmt (Store x es uk e) =
    do es' <- mapM cgExp es
       e' <- cgExp e
       let estore = cgStore x es' uk e'
       return $ C.Exp estore
cgStmt (Seq s1 s2) =
    do s1' <- cgStmt s1
       s2' <- cgStmt s2
       return $ C.Seq s1' s2'
cgStmt (If e s1 s2) =
    do e' <- cgExp e
       s1' <- cgStmt s1
       s2' <- cgStmt s2
       return $ C.If e' s1' (Just s2')
cgStmt (Loop _ x gen s) =
    do (e1', e2', e3') <- cgGen x gen
       s' <- cgStmt s
       return $ C.For e1' e2' e3' s'
cgStmt (MapRed acc x gen s e) =
    do (e1', e2', e3') <- cgGen x gen
       s' <- cgStmt s
       e' <- cgExp e
       let acc' = cgIdLhs acc
           sass = C.assignStmt acc' (C.Lit (C.Dbl 0))
           sacc = C.Exp (C.Assign (C.Var acc') C.PlusEqAss e')
       return $ C.Seq sass (C.For e1' e2' e3' (C.Seq s' sacc))



-----------------------------------
-- == Generate body

cgComp :: B.Comp (TVar Typ) -> CgM (C.Stmt (TVar C.Typ))
cgComp (B.Single _ s) = cgStmt s
cgComp (B.Block _ x gen s) =
    do let x' = C.Var $ cgIdLhs x
       (e1', e2') <- cgGenBnd gen
       -- only keep track of stuff we have parallelized
       s' <- withPllCtx (x, gen) (cgStmt s) 
       let idx = exp_BLOCKIDX_X * exp_BLOCKDIM_X + exp_THREADIDX_X
           s_idx = C.assignStmt' x' idx
           e_cond = (e1' `C.mkLte` x') `C.mkAnd` (x' `C.mkLt` e2')
       return $ C.Seq s_idx (C.If e_cond s' Nothing)
cgComp (B.Reduce _ _ x gen s _) =
    withPllCtx (x, gen) (cgStmt s)
       

-----------------------------------
-- == Generate call

cgGenBnd :: Gen (TVar Typ) -> CgM (C.Exp (TVar C.Typ), C.Exp (TVar C.Typ))
cgGenBnd (Until e1 e2) =
    do e1' <- cgExp e1
       e2' <- cgExp e2
       return (e1', e2')


cgCallSingle :: Name -> [C.Exp (TVar C.Typ)] -> CgM (C.Stmt (TVar C.Typ))
cgCallSingle name args =
    return $ C.Exp (C.KernelCall (nameToStr name) 1 1 args)

       
cgCallMap :: Bool -> Gen (TVar Typ) -> Name -> [C.Exp (TVar C.Typ)] -> CgM (C.Stmt (TVar C.Typ))
cgCallMap hasShared gen name args =
    do (e1, e2) <- cgGenBnd gen
       let blks = if hasShared then e2 - e1 else C.LibCall "BLOCKS" [ e2 - e1 ]
           thrds = if hasShared then 1 else C.mkLiterally "THREADS"
       return $ C.Exp (C.KernelCall (nameToStr name) blks thrds args)

              
cgCallMapRed :: Bool -> TVar Typ -> (Maybe (TVar Typ, Gen (TVar Typ))) -> Gen (TVar Typ) -> Name -> [C.Exp (TVar C.Typ)] -> CgM (C.Stmt (TVar C.Typ))
cgCallMapRed onDev tmp pllCtx gen name args =
    do (e1, e2) <- cgGenBnd gen
       let redArgs = [ C.LibCall "thrust::make_counting_iterator" [ e1 ]
                     , C.LibCall "thrust::make_counting_iterator" [ e2 ]
                     , C.LibCall (nameToStr name) args
                     , C.Lit $ C.Dbl 0
                     , C.LibCall "thrust::plus<double>" [] ]
           redArgs' = if onDev
                      then [ C.mkLiterally "thrust::device" ] ++ redArgs
                      else redArgs
           eCall = C.LibCall "thrust::transform_reduce" redArgs'
       genCtx pllCtx (C.assignStmt (cgIdLhs tmp) eCall)
    where                                 
      genCtx (Just (ctxIdx, ctxGen)) s =
          do (eAss', e1', e2') <- cgGen ctxIdx ctxGen
             return $ C.For eAss' e1' e2' s
      genCtx Nothing s = return s


-----------------------------------
-- == Generate call and kernel
                         
freshKernelName :: Name -> CgM Name
freshKernelName name =
    do genSym <- asks cr_genSym
       ctr <- lift $ lift $ readIORef genSym
       lift $ lift $ writeIORef genSym (ctr + 1)
       return $ mkCompName ("kernel_" ++ show ctr) name 
              

cgUnpackMcmcSt :: InferCtx (TVar Typ) -> Bool -> Map.Map (TVar Typ) Int -> [TVar Typ] -> CgM (C.Stmt (TVar C.Typ))
cgUnpackMcmcSt inferCtx useProp useCtx allocs =
    do v_rng <- asks cr_vRng
       v_aux <- asks cr_vAux
       v_curr <- asks cr_vCurr
       v_prop <- asks cr_vProp
       let modDecls = ic_modDecls inferCtx
           dupCtx = ic_dupCtx inferCtx
           s1 = unpackStrct useCtx v_aux (getModHyperIds modDecls)
           s2 = unpackStrct useCtx v_curr (getModParamIds modDecls)
           s3 = unpackStrct useCtx v_curr (getModDataIds modDecls)
           s4 = if useProp then unpackPropStrct v_prop dupCtx else C.mkSkip
           s5 = C.assignStmt v_rng (C.strctProj v_aux v_rng)
           v_idxs = cgIdLhs (getIdxVar inferCtx)
           s6 = C.assignStmt v_idxs (C.Cast (C.PtrTy ty_AugurVec) (C.addrOf (C.strctProj v_aux v_idxs)))
           s7 = unpackStrct useCtx v_aux allocs
       return $ C.seqStmt [ s1, s2, s3, s4, s5, s6, s7 ]

              
cgUnpackMcmcSt' :: InferCtx (TVar Typ) -> Bool -> Map.Map (TVar C.Typ) Int -> [TVar Typ] -> CgM (C.Stmt (TVar C.Typ))
cgUnpackMcmcSt' inferCtx useProp useCtx allocs =
    do v_aux <- asks cr_vAux
       v_curr <- asks cr_vCurr
       v_prop <- asks cr_vProp
       let modDecls = ic_modDecls inferCtx
           dupCtx = ic_dupCtx inferCtx
           s1 = unpackStrct' useCtx v_aux (getModHyperIds modDecls)
           s2 = unpackStrct' useCtx v_curr (getModParamIds modDecls)
           s3 = unpackStrct' useCtx v_curr (getModDataIds modDecls)
           s4 = if useProp then unpackPropStrct v_prop dupCtx else C.mkSkip
           s5 = unpackStrct' useCtx v_aux allocs
           v_idxs = cgIdLhs (getIdxVar inferCtx)
           s6 = C.assignStmt v_idxs (C.Cast (C.PtrTy ty_AugurVec) (C.addrOf (C.strctProj v_aux v_idxs)))
       return $ C.seqStmt [ s1, s2, s3, s4, s5, s6 ]

                        
cgCallAndKern :: InferCtx (TVar Typ) -> Bool -> Name -> [TVar Typ] -> [(TVar Typ, Typ)] -> Bool -> B.Comp (TVar Typ) -> CgM (C.Stmt (TVar C.Typ), [C.Decl (TVar C.Typ)])
cgCallAndKern inferCtx useProp name allocs params onDev comp =
    do body' <- cgComp comp
       let useCtx = cntVarUse comp
           idxs = map fst (filter (isGridIdx . idKind . fst) params)
       s_projIdx <- cgProjIdx idxs
       allocs' <- upVarTys allocs -- Typs could have changed
       s_unpack <- cgUnpackMcmcSt inferCtx useProp useCtx allocs'
       params' <- cgParams useProp params
       let body'' = C.seqStmt [ s_unpack, s_projIdx, body' ]
       name' <- freshKernelName name
       case comp of
         B.Single sc _ ->
             if onDev || sc == B.HostComp
             then return (body', [])
             else do let args = map (C.Var . fst) params'
                     call <- cgCallSingle name' args
                     return (call, [C.Fun [C.Global] name' params' body'' Nothing C.VoidTy])
         B.Block _ _ gen _ ->
             do let args = map (C.Var . fst) params'
                call <- cgCallMap False gen name' args
                return (call, [C.Fun [C.Global] name' params' body'' Nothing C.VoidTy])
         B.Reduce ctx acc redIdx redGen _ eRet ->
             do genSym <- asks cr_genSym
                tmp <- lift $ lift $ mkTId genSym Anon Local RealTy
                updateBody <- mkUpBody tmp acc                
                call <- cgCallMapRed onDev tmp ctx redGen name' (map (C.Var . fst) params')
                return (C.seqStmt [ call, updateBody ], [C.ThrustFunc name' (genParams ctx params') [(cgIdLhs redIdx, C.IntTy)] body'' (Just $ cgExpDet eRet) C.DblTy])
    where
      mkUpBody tmp (Var x) =
          return (C.assignStmt' (cgIdRhs x) (C.Var (cgIdLhs tmp)))
      mkUpBody tmp (Proj (Var x) es) =
          cgStmt (Store x es Update (Var tmp))
      mkUpBody _ e =
          error $ "[CudaC.CgCuda] | Shouldn't happen: " ++ pprShow e
                                 
      genParams (Just (ctxIdx, _)) params' =
          params' ++ [(cgIdLhs ctxIdx, C.IntTy)]
      genParams Nothing params' =
          params'


                                  
-----------------------------------
-- == Generate strategies

cgParams :: Bool -> [(TVar Typ, Typ)] -> CgM [(TVar C.Typ, C.Typ)]
cgParams useProp params =
    do v_aux <- asks cr_vAux
       v_curr <- asks cr_vCurr
       v_prop <- asks cr_vProp
       let -- Filter out "phantom" grid indices
           params' = filter (not . isGridIdx . idKind . fst) params
           params'' = map (\(x, ty) -> (cgIdLhs x, cgTyp Param ty)) params'           
           mcmcParams = if useProp
                        then [ (v_aux, ty_AugurAux), (v_curr, ty_AugurMod)
                             , (v_prop, ty_AugurMod) ]
                        else [ (v_aux, ty_AugurAux), (v_curr, ty_AugurMod) ]
       return $ mcmcParams ++ params''


cgDeclCPU :: InferCtx (TVar Typ) -> Map.Map (TVar Typ) Int -> Bool -> Decl (TVar Typ) -> CgM (C.Decl (TVar C.Typ))
cgDeclCPU inferCtx rtSizeCtx useProp (Fun name params allocs body retExp retTy) =
    do let idxs = map fst (filter (isGridIdx . idKind . fst) params)
       s_projIdx <- cgProjIdx idxs
       params' <- cgParams useProp params
       allocs' <- upVarTys allocs -- Typs could have changed
       s_unpack <- cgUnpackMcmcSt inferCtx useProp (cntVarUse body) allocs'
       body' <- cgStmt body
       let body'' = C.seqStmt [ s_unpack, s_projIdx, body' ]
       retExp' <- T.mapM cgExp retExp
       let retTy' = cgTyp Local retTy
       return $ C.Fun [] name params' body'' retExp' retTy'
    

cgDeclDev :: InferCtx (TVar Typ) -> Bool -> Decl (TVar Typ) -> CgM [C.Decl (TVar C.Typ)]
cgDeclDev inferCtx useProp (Fun name params allocs body retExp retTy) =
    do let idxs = map fst (filter (isGridIdx . idKind . fst) params)
       s_projIdx <- cgProjIdx idxs
       body' <- cgStmt body
       retExp' <- T.mapM cgExp retExp
       params' <- cgParams useProp params
       allocs' <- upVarTys allocs -- Typs could have changed
       s_unpack <- cgUnpackMcmcSt inferCtx useProp (cntVarUse body) allocs'
       let retTy' = cgTyp Local retTy
           body'' = C.seqStmt [ s_unpack, s_projIdx, body']
           decl' = C.Fun [C.Device] name params' body'' retExp' retTy'
       return [ decl' ]

              
cgDeclBlk :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> Map.Map (TVar Typ) Int -> Bool -> Bool -> Decl (TVar Typ) -> CgM [C.Decl (TVar C.Typ)]
cgDeclBlk cinfo copt inferCtx rtSizeCtx useProp onDev decl =
    do lift $ debugM "[CudaC.CgCuda]" $ "@cgDeclBlk | Input:\n" ++ pprShow decl
       decl' <- if getInline copt
                then lift $ runInlineDecl cinfo copt inferCtx decl
                else return decl
       let Fun name params allocs body retExp retTy = decl'
           comps = B.partitionStmt body
           paramTyCtx = Map.fromList params
           allocTyCtx = Map.fromList (map (\x -> (x, getType' x)) allocs)
           openCtx = paramTyCtx `Map.union` allocTyCtx
                     
       lift $ debugM "[CudaC.CgCuda]" $ "@cgDeclBlk | Blocks:\n" ++ pprShowLs' comps
       comps' <- if getSplitOnLoop copt
                 then mapM (\comp -> lift $ runSplitComp cinfo copt inferCtx rtSizeCtx openCtx comp) comps >>= return . concat
                 else return comps
       lift $ debugM "[CudaC.CgCuda]" $ "@cgDeclBlk | SplitLoop:\n" ++ pprShowLs' comps'
       shpCtx'' <- gets cs_shpCtx
       let shpCtx''' = Map.map (\(_, shp) -> shp) shpCtx''
       (shpCtxs, compss') <- lift $ mapM (splitAtmIncComp (getGenSym cinfo) shpCtx''' rtSizeCtx ) comps' >>= return . unzip
       let comps'' = concat compss'
           shpCtx = foldl (\acc ctx -> ctx `Map.union` acc) Map.empty shpCtxs
           allocs' = allocs ++ Map.keys shpCtx
       modify (\st -> st { cs_shpCtx = shpCtx `Map.union` cs_shpCtx st })
       lift $ debugM "[CudaC.CgCuda]" $ "@cgDeclBlk | SplitAtmInc:\n" ++ pprShowLs' comps''
       (calls, decls) <- mapM (cgCallAndKern inferCtx useProp name allocs' params onDev) comps'' >>= return . unzip

       {-
       let allocs' = allocs
       (calls, decls) <- mapM (cgCallAndKern inferCtx useProp name allocs params onDev) comps >>= return . unzip
       -}

       let idxs = map fst (filter (isGridIdx . idKind . fst) params)
       s_projIdx <- cgProjIdx idxs
       params' <- cgParams useProp params

       retExp' <- T.mapM cgExp retExp 
       let retTy' = cgTyp Param retTy
           attribs' = if onDev then [C.Device] else []
           body' = C.seqStmt calls
           useCtx = cntVarUse body'
       s_unpack <- cgUnpackMcmcSt' inferCtx useProp useCtx allocs'
       let body'' =  C.seqStmt [ s_unpack, s_projIdx, body']
           decl'' = C.Fun attribs' name params' body'' retExp' retTy'
       lift $ debugM "[CudaC.CgCuda]" $ "@cgDeclBlk | Output:\n" ++ pprShow decl''
       return $ concat decls ++ [decl'']

              
{-              
cgDeclRedStratGPU :: InferCtx (TVar Typ) -> Map.Map (TVar Typ) Int -> Bool -> LX.CallConv -> Decl (TVar Typ) -> CgM [C.Decl (TVar C.Typ)]
cgDeclRedStratGPU inferCtx rtSizeCtx useProp cc (Fun name params shared body retExp retTy) =
    do let comps = B.partitionStmt body
           comps' = concat $ map PB.runSplitComp comps
       comps'' <- mapM (PB.hoistComp rtSizeCtx) comps'
       -- traceM $ "Before red: " ++ sepByPy "\n\n" comps
       -- traceM $ "Split: " ++ sepByStr "\n\n" (map (\(x, c, e1, e2) -> show x ++ ", " ++ emitPy 0 e1 ++ ", " ++  emitPy 0 e2 ++ "\n" ++ emitPy 0 c) comps')
       -- traceM $ "After red: " ++ sepByPy "\n" comps''
       params' <- return $ map (\(v, t) -> (cgIdLhs v, cgTyp Param t)) params
       (calls, decls) <- mapM (cgCallAndKern inferCtx name shared params' False) comps'' >>= return . unzip
       retExp' <- T.mapM cgExp retExp 
       let retTy' = cgTyp Param retTy
           decl' = C.Fun [] name params' (C.seqStmt calls) retExp' retTy'
       return $ concat decls ++ [decl']
-}
              

-----------------------------------
-- == Top-level

cgDecl :: CompInfo -> CompOpt -> Target -> InferCtx (TVar Typ) -> Map.Map (TVar Typ) Int -> LX.LowMM (TVar Typ) -> CgM [C.Decl (TVar C.Typ)]
cgDecl cinfo copt target inferCtx rtSizeCtx (LX.LowMM (LX.LowXX _ useProp cc _ decl)) =
    case target of
      CPU -> cgDeclCPU inferCtx rtSizeCtx useProp decl >>= \decl' -> return [ decl' ]
      GPU BlkStrat ->
          case cc of
            LX.HostCall hostCode -> 
                  if hostCode
                  then do decl' <- cgDeclCPU inferCtx rtSizeCtx useProp decl
                          return [ decl' ]
                  else cgDeclBlk cinfo copt inferCtx rtSizeCtx useProp False decl
            LX.DevCall dynPar ->
                  if dynPar
                  then cgDeclBlk cinfo copt inferCtx rtSizeCtx useProp True decl
                  else cgDeclDev inferCtx useProp decl
      GPU RedStrat -> error "[CudaC.CgCuda] | TODO RedStrat??"
          -- cgDeclRedStratGPU inferCtx rtSizeCtx useProp cc decl
                      

initCgRdr :: CompInfo -> InferCtx (TVar Typ) -> Target -> TVar C.Typ -> CompM CgRdr
initCgRdr cinfo inferCtx target v_rng =
    do let genSym = getGenSym cinfo
       v_aux <- lift $ mkTyIdIO genSym Anon Local ty_AugurAux
       v_curr <- lift $ mkTyIdIO genSym Anon Local ty_AugurMod
       v_prop <- lift $ mkTyIdIO genSym Anon Local ty_AugurMod
       let target' = targetToExp target
       return $ CR v_rng (getIdxVar inferCtx) v_aux v_curr v_prop Nothing target' genSym

              
runCgDecl :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> Target -> Map.Map (TVar Typ) Int -> TVar C.Typ -> LX.LowMM (TVar Typ) -> CompM (S.ShpCtx (TVar Typ), [C.Decl (TVar C.Typ)])
runCgDecl cinfo copt inferCtx mode rtsizes v_rng lowmmDecl =
    do rdr <- initCgRdr cinfo inferCtx mode v_rng
       let shpCtx = LX.getGlobs (LX.unLowMM lowmmDecl)
           st = CS (Map.map (\_ -> False) shpCtx) (Map.mapWithKey (\x shp -> (getType' x, shp)) shpCtx)
       (decls, CS _ shpCtx', _) <- runRWST (cgDecl cinfo copt mode inferCtx rtsizes lowmmDecl) rdr st
       return (Map.map snd shpCtx', decls)


runCgDecls :: CompInfo -> CompOpt -> InferCtx (TVar Typ) -> Target -> Map.Map (TVar Typ) Int -> TVar C.Typ -> [LX.LowMM (TVar Typ)] -> CompM (S.ShpCtx (TVar Typ), [C.Decl (TVar C.Typ)])
runCgDecls cinfo copt inferCtx mode rtsizes v_rng decls =
    do (shpCtxs, ds) <- mapM (runCgDecl cinfo copt inferCtx mode rtsizes v_rng) decls >>= return . unzip
       let shpCtx = foldl (\acc shpCtx' -> acc `Map.union` shpCtx') Map.empty shpCtxs
       return $ (shpCtx, concat ds)
