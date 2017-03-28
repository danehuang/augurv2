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

module CudaC.CodeGenC
    ( runCgDecl) where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Traversable as T
import Debug.Trace
    
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

    
----------------------------------------------------------------------
-- = CodeGenC Description
{-| [Note]

This module contains code-generation to C-code.
TODO: A declaration's allocations are converted into additional 
entry/exit functions that allocate/deallocate?

Low M.Prim (TVar Typ)   (linerarized exp, types, simpl proj, allocs)
          | (codeGenC)
C (TVar C.Typ)          (linerarized exp, types, simpl proj, model state)
  + C (TVar C.Typ)      (allocs)

-}

compErr :: String -> a
compErr msg = compErrMod "CodeGenC" msg

-----------------------------------
-- == Types and operations

type CgM = RWST CgRdr [()] () CompM
data CgRdr =
    CR { cr_vRng :: TVar C.Typ
       , cr_vAux :: TVar C.Typ
       , cr_vCurr :: TVar C.Typ
       , cr_vProp :: TVar C.Typ
       , cr_genSym :: GenSym }

    
freshId :: Name -> IdKind -> t -> CgM (TVar t)
freshId name ik ty =
    do genSym <- asks cr_genSym
       lift $ lift $ mkTyIdIO genSym name ik ty 

                                                                                
-----------------------------------
-- == Transformations              

                       
cgRng :: [C.Exp (TVar C.Typ)] -> CgM [C.Exp (TVar C.Typ)]
cgRng es =
    do v_rng <- asks cr_vRng
       return $ C.Var v_rng : es

              
cgExp :: Exp (TVar Typ) -> CgM (C.Exp (TVar C.Typ))
cgExp (Var x) = return $ cgIdRhs x
cgExp (Lit lit) =
    case lit of
      Int i -> return $ C.mkInt i
      Real d -> return $ C.Lit (C.Dbl d)
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


cgMcmcCall :: Exp (TVar Typ) -> CgM (C.Stmt (TVar C.Typ))
cgMcmcCall (Call (PrimId _ _ (MWG prop swap mwg)) es) =
    do es' <- mapM cgExp es
       vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       vArr <- freshId Anon Local C.IntTy
       vView <- freshId Anon Local (C.VecTy C.IntTy)
       vIdxs <- freshId Anon Local (C.PtrTy (C.VecTy C.IntTy))
       let idxs = drop 3 es'
           s_arr = C.Declare (C.ConstArr vArr (length idxs)) (Just (C.Lit (C.Array idxs)))
           es_viewArgs = [ C.mkLiterally enum_AUGUR_DBL
                         , C.Var vArr
                         , C.mkInt (length idxs) ]
           s_view = C.Exp $ C.Assign (C.Var vView) C.EqAss (C.LibCall "augur_arr_view_as_vec" es_viewArgs)
           s_idxs = C.Exp $ C.Assign (C.Var vIdxs) C.EqAss (C.addrOf (C.Var vView))
           v_ll = case es !! 2 of
                    Var x -> cgIdLhs x
                    _ -> compErr $ "HAXORZ alert"
           es'' = [ C.Var vAux, C.Var vCurr, C.Var vProp, C.Var v_ll, C.Var vIdxs, C.mkLiterally (nameToStr prop), C.mkLiterally (nameToStr swap), C.mkLiterally (nameToStr mwg) ]
           s_call = C.Exp $ C.LibCall "h_augur_mcmc_mwg" es''
       return $ C.seqStmt [ s_arr, s_view, s_idxs, s_call ]
cgMcmcCall (Call (PrimId _ _ (EllipSlice likeOne)) es) =
    do es' <- mapM cgExp es
       vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       vArr <- freshId Anon Local (C.IntTy)
       vView <- freshId Anon Local (C.VecTy C.IntTy)
       vIdxs <- freshId Anon Local (C.PtrTy (C.VecTy C.IntTy))
       let idxs = drop 6 es'
           s_arr = C.Declare (C.ConstArr vArr (length idxs)) (Just (C.Lit (C.Array idxs)))
           s_view = C.Exp $ C.Assign (C.Var vView) C.EqAss (C.LibCall "augur_arr_view_as_vec" [ C.mkLiterally enum_AUGUR_DBL, C.Var vArr, C.mkInt (length idxs) ])
           s_idxs = C.Exp $ C.Assign (C.Var vIdxs) C.EqAss (C.addrOf (C.Var vView))
           es'' = [ C.Var vAux, C.Var vCurr, C.Var vProp ] ++ (take 6 es') ++ [ C.Var vIdxs, C.Lit (C.Literally (nameToStr likeOne)) ]
           s_call = C.Exp $ C.LibCall "augur_mcmc_eslice" es''
       return $ C.seqStmt [ s_arr, s_view, s_idxs, s_call ]
cgMcmcCall (Call (PrimId _ _ (LeapFrog grad prop)) es) =
    do es' <- mapM cgExp es
       vAux <- asks cr_vAux
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       let es'' = [ C.Lit (C.Literally "AUGUR_CPU"), C.Var vAux, C.Var vCurr, C.Var vProp ] ++ es' ++ [ C.Lit (C.Literally (nameToStr grad)), C.Lit (C.Literally (nameToStr prop)) ]
           s_call = C.Exp $ C.LibCall "h_augur_mcmc_hmc" es''
       return s_call
cgMcmcCall _ =
    return $ C.mkSkip
       
       
-- TODO: UGHHH Refactor me             
cgStmt :: Stmt (TVar Typ) -> CgM (C.Stmt (TVar C.Typ))
cgStmt Skip =
    return $ C.mkSkip
cgStmt (Exp e) =
    case e of
      Call (PrimId _ _ (MWG _ _ _)) _ ->  cgMcmcCall e
      Call (PrimId _ _ (EllipSlice _)) _ -> cgMcmcCall e
      Call (PrimId _ _ (LeapFrog _ _)) _ -> cgMcmcCall e
      _ ->
          do e' <- cgExp e
             return $ C.Exp e'
cgStmt (Assign x e) =
    case e of
      Call (PrimId _ _ ReadVecFromShape) es ->
          do v_view <- freshId Anon Local ty_AugurVec 
             es' <- mapM cgExp es
             let s1 = C.assignStmt v_view (C.LibCall "augur_vec_view_as" es')
                 s2 = C.assignStmt' (C.Var (cgIdLhs x)) (C.addrOf (C.Var v_view))
             return $ C.seqStmt [ s1, s2 ]
      Call (PrimId _ _ ReadMatFromShape) es ->
          do v_view <- freshId Anon Local ty_AugurMat
             es' <- mapM cgExp es
             let s1 = C.assignStmt v_view (C.LibCall "augur_mat_view_as" es')
                 s2 = C.assignStmt' (C.Var (cgIdLhs x)) (C.addrOf (C.Var v_view))
             return $ C.seqStmt [ s1, s2 ]
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

              
cgDeclMain :: InferCtx (TVar Typ) -> LX.LowMM (TVar Typ) -> CgM (C.Decl (TVar C.Typ))
cgDeclMain inferCtx (LX.LowMM (LX.LowXX _ useProp cc projIdx decl)) =
    do let Fun name params allocs body retExp retTy = decl
       v_rng <- asks cr_vRng
       v_aux <- asks cr_vAux
       v_curr <- asks cr_vCurr
       v_prop <- asks cr_vProp
       params' <- mapM (\(x, ty) -> return (cgIdLhs x, cgTyp (idKind x) ty)) params
       body' <- cgStmt body
       let useCtx = cntVarUse body
           mcmcParams = if useProp
                        then [ (v_aux, ty_AugurAux), (v_curr, ty_AugurMod)
                             , (v_prop, ty_AugurMod) ]
                        else [ (v_aux, ty_AugurAux), (v_curr, ty_AugurMod) ]
           params'' = mcmcParams ++ params'
           modDecls = ic_modDecls inferCtx
           dupCtx = ic_dupCtx inferCtx
           s1 = unpackStrct useCtx v_aux (getModHyperIds modDecls)
           s2 = unpackStrct useCtx v_curr (getModParamIds modDecls)
           s3 = unpackStrct useCtx v_curr (getModDataIds modDecls)
           s4 = if useProp then unpackPropStrct v_prop dupCtx else C.mkSkip
           s5 = C.assignStmt v_rng (C.strctProj v_aux v_rng)
           s6 = unpackStrct useCtx v_aux allocs
       let body'' = C.seqStmt [ s1, s2, s3, s4, s5, s6, body' ]
       retExp' <- T.mapM cgExp retExp
       let retTy' = cgTyp Local retTy
       return $ C.Fun [] name params'' body'' retExp' retTy'


cgDecl :: InferCtx (TVar Typ) -> LX.LowMM (TVar Typ) -> CgM [C.Decl (TVar C.Typ)]
cgDecl inferCtx lowmmDecl =    
    do decl2 <- cgDeclMain inferCtx lowmmDecl
       return [ decl2 ]
            
                   
-----------------------------------
-- == Top-level

initCgRdr :: CompInfo -> TVar C.Typ -> CompM CgRdr
initCgRdr cinfo v_rng =
    do let genSym = getGenSym cinfo
       v_aux <- lift $ mkTyIdIO genSym Anon Local ty_AugurAux
       v_curr <- lift $ mkTyIdIO genSym Anon Local ty_AugurMod
       v_prop <- lift $ mkTyIdIO genSym Anon Local ty_AugurMod
       return $ CR v_rng v_aux v_curr v_prop genSym

              
runCgStmt :: CompInfo -> TVar C.Typ -> Stmt (TVar Typ) -> CompM (C.Stmt (TVar C.Typ))
runCgStmt cinfo v_rng stmt =
    do rdr <- initCgRdr cinfo v_rng
       (stmt', _, _) <- runRWST (cgStmt stmt) rdr ()
       return stmt'

              
runCgDecl :: CompInfo -> InferCtx (TVar Typ) -> TVar C.Typ -> LX.LowMM (TVar Typ) -> CompM [C.Decl (TVar C.Typ)]
runCgDecl cinfo inferCtx v_rng lowmmDecl =
     do rdr <- initCgRdr cinfo v_rng
        (decl', _, _) <- runRWST (cgDecl inferCtx lowmmDecl) rdr ()
        return decl'

