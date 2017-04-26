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


module CudaC.CgKern
    (runCgMcmcDecl) where

import Control.Monad.Reader
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Var
import Compile.CompData
import Core.KernSyn
import Core.CgDet
import Low.LowSyn
import Core.CoreTySyn
import qualified CudaC.CgCudaCCore as CCG
import qualified CudaC.CudaCSyn as C
import qualified Core.Graph as G
import CudaC.XfaceCudaC
    

----------------------------------------------------------------------
-- = CgKern Description
{-| [Note]

Generates native MCMC kernel.

Invariant:
curr (contains current state)
prop (contains proposal state)
curr = prop after every step

-}
              
cgId :: TVar Typ -> TIdCTy
cgId = CCG.cgIdLhs


prjev :: C.Exp TIdCTy -> TIdCTy -> C.Exp TIdCTy
prjev e v =
    C.Binop e C.Proj (C.Var v)


-----------------------------------
-- == Types and operations

type TIdTy = TVar Typ
type TIdCTy = TVar C.Typ
type CgM = ReaderT CgRdr CompM

data CgRdr =
    CR { cgr_curr :: C.Exp TIdCTy
       , cgr_prop :: C.Exp TIdCTy
       , cgr_aux :: C.Exp TIdCTy
       , cgr_auxLL :: TIdCTy
       , cgr_needProp :: Bool
       , cgr_loc :: C.Exp TIdCTy
       , cgr_depG :: G.Graph TIdTy
       }

     
-----------------------------------
-- == Transformations

genReset' :: C.Exp TIdCTy -> C.Exp TIdCTy -> TIdTy -> C.Exp TIdCTy
genReset' e_loc e_aux vCi =
    case getType' vCi of
      VecTy (VecTy _) -> C.LibCall "h_augur_flat_vec_zero" args
      VecTy (MatTy _) -> C.LibCall "h_augur_flat_vec_zero" args
      VecTy IntTy -> C.LibCall "h_augur_flat_vec_zero" args
      VecTy RealTy -> C.LibCall "h_augur_flat_vec_zero" args
      MatTy _ -> C.LibCall "h_augur_mat_zero" args
      IntTy -> C.LibCall "h_augur_zeroi" args'
      RealTy -> C.LibCall "h_augur_zerod" args'
      t -> error $ "[CgKern] @genRest' | Resetting type " ++ show t ++ " not supported"
    where
      args = [ e_loc, C.addrOf (prjev e_aux $ cgId vCi) ]
      args' = [ e_loc, prjev e_aux $ cgId vCi ]
             

genReset :: C.Exp TIdCTy -> C.Exp TIdCTy -> TIdTy -> Bool -> C.Stmt TIdCTy
genReset e_loc e_aux vCi reset =
    if reset
    then C.Exp (genReset' e_loc e_aux vCi)
    else C.Exp (C.mkInt 0)

         
cgReset :: C.Exp TIdCTy -> C.Exp TIdCTy -> [(TIdTy, AllocKind)] -> [C.Stmt TIdCTy]
cgReset e_loc e_aux = map f
    where
      f (v, Reset) = genReset e_loc e_aux v True
      f (v, Work) = genReset e_loc e_aux v False

askMcmcRepr :: CgM (C.Exp TIdCTy, C.Exp TIdCTy, C.Exp TIdCTy)
askMcmcRepr =
    do e_curr <- asks cgr_curr
       e_prop <- asks cgr_prop
       e_aux <- asks cgr_aux
       return (e_curr, e_prop, e_aux)


cgUserProp :: [(TIdTy, AllocKind)] -> PropKind (Decl TIdTy) -> CgM (C.Stmt TIdCTy)
cgUserProp allocs pk =
    case pk of
      Joint _ -> error $ "cgUserProp TODO"
      MWG _ _ _ mwg ->
          do (e_curr, e_prop, e_aux) <- askMcmcRepr
             return $ C.Exp (C.LibCall (declName' mwg) [ e_aux, e_curr, e_prop])

               
cgGradProp :: [(TIdTy, AllocKind)] -> [TyId] -> GradKind (Decl TIdTy) -> CgM (C.Stmt TIdCTy)
cgGradProp allocs kernParams pk = 
    case pk of
      HMC grad prop simLen stepSize ->
          do (e_curr, e_prop, e_aux) <- askMcmcRepr
             let e_simLen = C.strctProj'' e_aux (C.Var (cgId (kernParams !! 0)))
                 e_stepSize = C.strctProj'' e_aux (C.Var (cgId (kernParams !! 1)))
             return $ C.mkLibCall (declName' prop) [ e_aux, e_curr, e_prop, e_simLen, e_stepSize ]
      NUTS grad prop stepSize ->
          do (e_curr, e_prop, e_aux) <- askMcmcRepr
             let  e_stepSize = C.strctProj'' e_aux (C.Var (cgId (kernParams !! 0)))
             return $ C.mkLibCall (declName' prop) [ e_aux, e_curr, e_prop, e_stepSize ]
      Reflect grad prop simLen stepSize ->
          do (e_curr, e_prop, e_aux) <- askMcmcRepr
             let e_simLen = C.strctProj'' e_aux (C.Var (cgId (kernParams !! 0)))
                 e_stepSize = C.strctProj'' e_aux (C.Var (cgId (kernParams !! 1)))
             return $ C.mkLibCall (declName' prop) [ e_aux, e_curr, e_prop, e_simLen, e_stepSize ]

cgGibbs :: [(TIdTy, AllocKind)] -> GibbsKind (Decl TIdTy) -> TyId -> C.Exp (TVar C.Typ) -> CgM (C.Stmt TIdCTy)
cgGibbs allocs gk v_mod loc =
    case gk of
      Disc samp ->
          do (e_curr, _, e_aux) <- askMcmcRepr
             let args = [ e_aux, e_curr ]
                 s_samp = C.mkLibCall (declName' samp) args
             s_upProp <- cgUpProp
             return $ C.seqStmt [ s_samp, s_upProp ]
      Conj stat samp -> 
          do (e_curr, _, e_aux) <- askMcmcRepr
             e_loc <- asks cgr_loc
             let args = [ e_aux, e_curr ]
                 resets = cgReset e_loc e_aux allocs
                 s_call = C.Exp (C.LibCall (declName' stat) args)
                 s_samp = C.Exp (C.LibCall (declName' samp) args)
             s_upProp <- cgUpProp
             return $ C.seqStmt (resets ++ [ s_call, s_samp, s_upProp ])
    where
      v_mod' = cgId v_mod
      
      cgUpProp =
          do (e_curr, e_prop, _) <- askMcmcRepr
             let args = [ loc
                        , C.addrOf (C.strctProj'' e_prop (C.Var v_mod'))
                        , C.addrOf (C.strctProj'' e_curr (C.Var v_mod')) ]
             b <- asks cgr_needProp
             if b
             then case getType' v_mod of
                    IntTy -> return $ C.mkLibCall "h_augur_basety_cpy_data" (args ++ [ C.mkLiterally (ty2AugurTy IntTy) ])
                    RealTy -> return $ C.mkLibCall "h_augur_basety_cpy_data" (args ++ [ C.mkLiterally (ty2AugurTy RealTy) ])
                    VecTy _ -> return $ C.mkLibCall "h_augur_flat_vec_cpy_data" args
                    MatTy _ -> return $ C.mkLibCall "h_augur_mat_cpy_data" args
                    ty -> error $ "[CgKern] @cgUpProp | Shouldn't happen: " ++ pprShow ty
             else return C.mkSkip
          


cgSlice :: [(TIdTy, AllocKind)] -> SliceKind (Decl TIdTy) -> CgM (C.Stmt TIdCTy)
cgSlice allocs sk =
    case sk of
      Ellip _ prop ->
          do (e_curr, e_prop, e_aux) <- askMcmcRepr
             let args = [ e_aux, e_curr, e_prop ]
                 s_call = C.Exp (C.LibCall (declName' prop) args)
             return s_call

               
cgMcmcBody :: Kern (Decl TIdTy) TIdTy -> CgM (C.Stmt TIdCTy)
cgMcmcBody (Base kind ku fc allocs kernParams code) =
    case kind of
      UserProp pk ->
          do s <- cgUserProp allocs pk
             emitDet s (kuVars ku)
      GradProp gk ->
          do s <- cgGradProp allocs kernParams gk
             emitDet s (kuVars ku)
      Gibbs gk ->
          do loc <- asks cgr_loc
             s <- cgGibbs allocs gk (head (kuVars ku)) loc
             emitDet s (kuVars ku)
      Slice sk ->
          do s <- cgSlice allocs sk
             emitDet s (kuVars ku)
    where
      emitDet s vs =
          do s_dets <- mapM updateDet vs
             return $ C.seqStmt (s : s_dets)
      
      updateDet v =
          do depG <- asks cgr_depG
             (e_curr, _, e_aux) <- askMcmcRepr
             let verts = G.verticesOf depG v
                 dets = filter (\v' -> case idKind v' of
                                        ModParam PK_Det -> True
                                        _ -> False) verts
                 args = [ e_aux, e_curr ]
                 s_dets = map (\v' -> C.mkLibCall (nameToStr (detName v')) args) dets
             return $ C.seqStmt s_dets
cgMcmcBody (Tensor k1 k2) =
    do s1 <- cgMcmcBody k1
       s2 <- cgMcmcBody k2
       return $ C.Seq s1 s2

              
-----------------------------------
-- == Top-level
                          
cgMcmcDecl :: Kern (Decl TIdTy) TIdTy -> CgM (C.Decl TIdCTy)
cgMcmcDecl kern =
    do v_auxLL <- asks cgr_auxLL
       let s_aux = C.assignStmt' (C.Var v_auxLL) 0
       body <- cgMcmcBody kern
       let body' = C.seqStmt [ s_aux, body ]
       return $ C.Fun [C.Extern] (mkName "augur_step") [] body' Nothing C.VoidTy

              
runCgMcmcDecl :: CompInfo -> Target -> G.Graph TIdTy -> Kern (Decl TIdTy) TIdTy -> CompM (C.Decl TIdCTy)
runCgMcmcDecl cinfo target depG kern =
    do let genSym = getGenSym cinfo
       v_auxLL <- lift $ mkTyIdIO genSym Anon Local C.DblTy
       -- arCtx <- runARLatKern kern
       let rdr = CR exp_MCMC_CURR exp_MCMC_PROP exp_MCMC_AUX v_auxLL (needProp kern) (targetToExp target) depG
       runReaderT (cgMcmcDecl kern) rdr



                  

{-
-----------------------------------
-- == Accept/Reject lattice

data ARLat = AR
           | Always
             deriving (Show)

type LatM m b = StateT (Map.Map b ARLat) m

data Direction = Forward | Backward
    
joinARLat :: ARLat -> ARLat -> ARLat
joinARLat AR AR = AR
joinARLat Always AR = AR
joinARLat AR Always = AR
joinARLat Always Always = Always

                          
modifyARLat :: (Monad m, BasicVar b) => b -> ARLat -> LatM m b ()
modifyARLat v ar =
    modify (\st -> case Map.lookup v st of
                     Just ar' -> Map.insert v (joinARLat ar ar') st
                     Nothing -> Map.insert v ar st)

                                        
arLatKern :: (Monad m, BasicVar b) => Kern c b -> LatM m b ()
arLatKern (Base kind ku _ _ _ _) =
    case kind of
      UserProp _ -> mapM_ (\v -> modifyARLat v AR) (kuVars ku)
      GradProp _ -> mapM_ (\v -> modifyARLat v AR) (kuVars ku)
      Gibbs _ -> mapM_ (\v -> modifyARLat v Always) (kuVars ku)
      Slice _ -> mapM_ (\v -> modifyARLat v AR) (kuVars ku)
arLatKern (Tensor k1 k2) =
    do arLatKern k1
       arLatKern k2


runARLatKern :: (Monad m, BasicVar b) => Kern c b -> m (Map.Map b ARLat)
runARLatKern kern =
    do (_, arCtx) <- runStateT (arLatKern kern) Map.empty
       return arCtx
-}
