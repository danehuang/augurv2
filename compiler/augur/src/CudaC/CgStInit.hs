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


module CudaC.CgStInit
    ( runCgStInit
    , runCgStSetPt
    , runCgStCpy
    , runCgCallLLMod ) where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as Map
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Pretty
import AstUtil.Var
import Core.CoreSyn
import Core.KernSyn
import CudaC.XfaceCudaC
import Compile.CompData
import qualified CudaC.CudaCSyn as C
import qualified Low.LowShpSyn as S
import Core.CoreTySyn
import qualified CudaC.CgCudaCCore as CCG
    

----------------------------------------------------------------------
-- = CgStInit Description
{-| [Note]

-}



-----------------------------------
-- == Imports
        
cgId :: TVar Typ -> TVar C.Typ
cgId = CCG.cgIdLhs
        

       
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

cgTypXfer :: IdKind -> Typ -> C.Typ
cgTypXfer ik ty =
    case ty of
      UnitTy -> C.VoidTy
      IntTy -> if isIndirect ik then C.PtrTy C.IntTy else C.IntTy
      RealTy -> if isIndirect ik then C.PtrTy C.DblTy else C.DblTy          
      VecTy _ -> ty_AugurVec
      MatTy _ -> ty_AugurMat
      BlkTy _ -> ty_AugurBlk
      _ -> error $ "Cannot code-gen arrow type."
                                           

cgExpXfer :: TVar Typ -> C.Exp (TVar C.Typ) -> C.Exp (TVar C.Typ)
cgExpXfer x e =
    case getType' x of
      IntTy -> e   -- TODO: isIndirect?
      RealTy -> e  -- TODO: isIndirect?
      VecTy _ -> C.addrOf e
      MatTy _ -> C.addrOf e
      BlkTy _ -> C.addrOf e
      ty -> error $ "Shouldn't happen " ++ pprShow ty
       

cgConstExp :: S.ConstExp (TVar Typ) ->  C.Exp (TVar C.Typ)
cgConstExp (S.Var x) = C.Var (cgId x)
cgConstExp (S.Int i) = C.mkInt i


cgShpId :: TVar Typ -> C.Exp (TVar C.Typ)
cgShpId x =
    case getType' x of
      IntTy -> C.Var (cgId x)
      RealTy -> C.Var (cgId x)
      VecTy _ -> C.addrOf (C.Var (cgId x))
      MatTy _ -> C.addrOf (C.Var (cgId x))
      ty -> error $ "Shouldn't happen " ++ pprShow ty

            
cgShpExpLit :: S.ShpExp (TVar Typ) -> C.Lit (TVar C.Typ)
cgShpExpLit (S.Cpy x) =
    C.Strct [ ("kind", C.mkLiterally "DIM_CPY")
            , ("cpyty", C.mkLiterally (ty2AugurTy (getType' x)))
            , ("cpyof", cgShpId x) ]
cgShpExpLit (S.Val e) =
    C.Strct [ ("kind", C.mkLiterally "DIM_VAL")
            , ("val", cgConstExp e) ]
cgShpExpLit (S.MaxDim x axis) =
    C.Strct [ ("kind", C.mkLiterally "DIM_MAX")
            , ("objty", C.mkLiterally enum_AUGUR_VEC)
            , ("obj", cgShpId x)
            , ("dim", C.mkInt axis) ]

    
cgShpLit :: S.Shp (TVar Typ) -> [C.Lit (TVar C.Typ)]
cgShpLit S.Scalar = []
cgShpLit (S.SingConn e shp') = cgShpExpLit e : cgShpLit shp'
cgShpLit (S.MatConn row col shp') =
    C.Strct [ ("kind", C.mkLiterally "DIM_MAT")
            , ("row", callShpExp row)
            , ("col", callShpExp col) ] : cgShpLit shp'
cgShpLit shp@(S.BlkOf _) = error $ "[CgStInit] | TODO " ++ pprShow shp

    
callShpExp :: S.ShpExp (TVar Typ) -> C.Exp (TVar C.Typ)
callShpExp (S.Cpy v) =
    C.addrOf (C.Var (cgId v))
callShpExp (S.Val ce) =
    cgConstExp ce
callShpExp (S.MaxDim v i) =
    case getType' v of
      VecTy _ -> C.LibCall "h_augur_vec_maxdim" args
      MatTy _ -> C.LibCall "h_augur_mat_dim" args
      ty -> error $ "[CgStInit] @cgShpExp | Cannot take maxdim of " ++ pprShow ty
    where
      args = [ C.addrOf (C.Var (cgId v)), C.mkInt i ]


cgTypArr :: [TVar Typ] -> C.Exp (TVar C.Typ)
cgTypArr vs =
    C.Lit (C.Array (map (C.mkLiterally . ty2AugurTy . getType') vs))


cgChkNative :: C.Exp (TVar C.Typ) -> TVar C.Typ -> TVar Typ -> C.Stmt (TVar C.Typ)
cgChkNative loc v_strct v =
    case getType' v of
      IntTy -> f C.mkSkip   -- TODO: non-essential
      RealTy -> f C.mkSkip  -- TODO: non-essential
      -- f (C.mkLibCall "h_augur_dbl_from_rtval" [ loc, C.derefStrct v_strct v' ])
      VecTy _ -> f (C.mkLibCall "h_augur_flat_vec_dump" [ loc, C.addrOf (C.derefStrct v_strct v') ])          
      MatTy _ -> f (C.mkLibCall "h_augur_mat_dump" [ loc, C.addrOf (C.derefStrct v_strct v') ])
      ty -> error $ "[CgStInit] @cgChkNative | Cannot check native allocation for type " ++ pprShow ty
    where
      f s = C.Seq (C.mkLibCall "printf" [ C.Lit (C.CString (pprShow v ++ "\\n")) ]) s
      v' = cgId v

           
{-| [Note] Allocate auxilliary memory

Case[Scalar]:
  v_strct->v = augur_malloc(sizeof(base_ty), loc);

Case[BlkOf[v_1:t_1, .., v_n:t_n]]:
  v_typs[n] = { t_1, .., t_n };
  v_blks[n] = { &v_1, .., &v_n };
  h_augur_blk_mk_cpy(LOC, &v_strct->v, n, v_typs, v_blks)

Case[se_1, .., se_n]:
  v_dims = { cg(se_1), .., cg(se_n) };
  v_shp = h_augur_shape_stk_alloc(v_dims, n, base_ty);
  h_augur_rtval_from_shape(LOC, &v_strct->v, &v_shp);
  h_augur_shape_stk_free(&v_shp);

-}
cgAuxShp :: Bool -> C.Exp (TVar C.Typ) -> TVar C.Typ -> TVar Typ -> S.Shp (TVar Typ) -> CgM (C.Stmt (TVar C.Typ))
cgAuxShp chk loc v_strct v shp =
    case shp of
      S.Scalar -> 
          case getType' v of
            IntTy ->
                do let e_args = [ C.mkLiterally "sizeof(int)", loc ]
                       e_alloc = C.LibCall "augur_malloc" e_args
                   return $ C.assignStmt' dst (C.Cast (C.PtrTy C.IntTy) e_alloc)
            RealTy ->                
                do let e_args = [ C.mkLiterally "sizeof(double)", loc ]
                       e_alloc = C.LibCall "augur_malloc" e_args
                   return $ C.assignStmt' dst (C.Cast (C.PtrTy C.DblTy) e_alloc)
            ty -> error $ "[CgStInit] | Cannot allocated " ++ pprShow v ++ " " ++ " with shape " ++ pprShow shp ++ " and type " ++ pprShow ty
      S.BlkOf _ -> return C.mkSkip
      _ ->
          do v_dims <- freshId Anon Local ty_AugurDim
             v_shp <- freshId Anon Local ty_AugurShape
             let numdims = S.lenShp shp
                 e_dims = C.Lit (C.Array (map C.Lit (cgShpLit shp)))
                 s_dims = C.Declare (C.ConstArr v_dims numdims) (Just e_dims)
                 e_args = [ C.Var v_dims
                          , C.mkInt numdims
                          , C.mkLiterally (ty2AugurTy (baseTy (getType' v))) ] -- TODO: Hacked typ?
                 s_shp = C.assignStmt v_shp (C.LibCall "h_augur_shape_stk_alloc" e_args)                 
                 e_args' = [ loc, cgExpXfer v dst, C.addrOf (C.Var v_shp) ]
                 s_xfer = C.mkLibCall "h_augur_rtval_from_shape" e_args'
                 s_free = C.mkLibCall "h_augur_shape_stk_free" [ C.addrOf (C.Var v_shp) ]
                 s_chk = if chk then cgChkNative loc v_strct v else C.mkSkip
             return $ C.seqStmt [ s_dims, s_shp, s_xfer, s_free, s_chk ]
    where
      v' = cgId v
      dst = C.derefStrct v_strct v'

      baseTy IntTy = IntTy
      baseTy RealTy = RealTy
      baseTy (VecTy ty) = baseTy ty
      baseTy (MatTy ty) = ty
      baseTy _ = error $ "[CgStInit] | Shouldn't happen"
            
                    
{-| [Note] Group already existing parameters into a block.

Suppose v_blk = [ v_1:t_1, .., v_n:t_n ]:

Then the code looks like:

v_typs = { t_1, .., t_n };
v_blks = { &v_strct.v_1, .., &v_strct.v_n };
h_augur_blk_mk_group(&v_strct.v_blk, n, v_typs, v_blks);

-}               
cgXferBlk :: TVar C.Typ -> [TVar Typ] -> TVar Typ -> CgM (C.Stmt (TVar C.Typ))
cgXferBlk v_strct vs v_blk =
    do v_typs <- freshId Anon Local ty_AugurTyp
       v_blks <- freshId Anon Local (C.PtrTy C.VoidTy)
       let numblks = length vs
           e_typs = cgTypArr vs
           s_typs = C.Declare (C.ConstArr v_typs numblks) (Just e_typs)
           vs' = map (\v -> cgExpXfer v (C.derefStrct v_strct (cgId v))) vs
           e_blks = C.Lit (C.Array vs')
           s_blks = C.Declare (C.ConstArr v_blks numblks) (Just e_blks)
           e_args = [ cgExpXfer v_blk (C.derefStrct v_strct v_blk')
                    , C.mkInt numblks
                    , C.Var v_typs
                    , C.Var v_blks ]
           s_call = C.mkLibCall "h_augur_blk_mk_group" e_args
       return $ C.seqStmt [ s_typs, s_blks, s_call ]
    where
      v_blk' = cgId v_blk
               
               
cgXferToNative :: C.Exp (TVar C.Typ) -> TVar C.Typ -> Bool -> TVar Typ -> C.Stmt (TVar C.Typ)
cgXferToNative loc v_strct prmBase v =
    case getType' v of
      IntTy ->
          if prmBase
          then
              let es_args = [ loc, C.Var v', exp_TRUE]
              in
                C.assignStmt' e_dst (C.LibCall "h_augur_rtval_from_int" es_args)
          else C.assignStmt' e_dst (C.Var v')
      RealTy ->
          if prmBase
          then
              let es_args = [ loc, C.Var v', exp_TRUE ]
              in
                C.assignStmt' e_dst (C.LibCall "h_augur_rtval_from_dbl" es_args)
          else C.assignStmt' e_dst (C.Var v')
      VecTy _ ->
          let es_args = [ loc, C.addrOf e_dst, C.addrOf (C.Var v'), exp_TRUE ]
          in
            C.mkLibCall "h_augur_rtval_from_vec" es_args
      MatTy _ ->
          let es_args = [ loc, C.addrOf e_dst, C.addrOf (C.Var v'), exp_TRUE ]
          in
            C.mkLibCall "h_augur_rtval_from_mat" es_args
      BlkTy _ -> C.mkSkip
      ty -> error $ "[CgInitSt] @cgXfer | Type " ++ pprShow ty ++ " not supported for variable " ++ pprShow v
    where
      v' = cgId v
      e_dst = C.derefStrct v_strct v'


cgXferFromNative :: C.Exp (TVar C.Typ) -> TVar C.Typ -> TVar Typ -> C.Stmt (TVar C.Typ)
cgXferFromNative loc vStrct v =
    case getType' v of
      IntTy ->
          let es_args = [ loc, C.Var v', e_src ]
          in
            C.mkLibCall "h_augur_int_from_native" es_args
      RealTy ->
          let es_args = [ loc, C.Var v', e_src ]
          in
            C.mkLibCall "h_augur_dbl_from_native" es_args
      VecTy _ ->
          let es_args = [ loc, C.addrOf (C.Var v'), C.addrOf e_src ]
          in
            C.mkLibCall "h_augur_vec_from_native" es_args
      MatTy _ ->
          let es_args = [ loc, C.addrOf (C.Var v'), C.addrOf e_src ]
          in
            C.mkLibCall "h_augur_rtval_from_mat" es_args
      BlkTy _ -> C.mkSkip
      ty -> error $ "[CodeGenC] @cgMcmcCpy | Type not supported: " ++ pprShow ty
    where
      v' = cgId v
      e_src = C.derefStrct vStrct v'


cgCpyBlk :: C.Exp (TVar C.Typ) -> TVar C.Typ -> TVar Typ -> [TVar Typ] -> CgM (C.Stmt (TVar C.Typ))
cgCpyBlk loc v_strct v vs =
    do v_typs <- freshId Anon Local ty_AugurTyp
       v_blks <- freshId Anon Local (C.PtrTy C.VoidTy)
       let numblks = length vs
           e_typs = cgTypArr vs
           s_typs = C.Declare (C.ConstArr v_typs numblks) (Just e_typs)
           e_blks = C.Lit (C.Array (map (cgShpId) vs))
           s_blks = C.Declare (C.ConstArr v_blks numblks) (Just e_blks)
           e_args = [ loc
                    , cgExpXfer v dst
                    , C.mkInt numblks
                    , C.Var v_typs
                    , C.Var v_blks ]
           s_call = C.mkLibCall "h_augur_blk_mk_cpy" e_args
       return $ C.seqStmt [ s_typs, s_blks, s_call ]
    where
      v' = cgId v
      dst = C.derefStrct v_strct v'

            
cgBlkKernAux :: S.ShpCtx (TVar Typ) -> C.Exp (TVar C.Typ) -> TVar C.Typ -> Kern code (TVar Typ) -> CgM (C.Stmt (TVar C.Typ))
cgBlkKernAux shpCtx loc v_strct = go
    where
      go (Base kind _ _ allocs _ _) =
          case kind of
            GradProp (HMC _ _ _ _) ->
                do traceM $ "ALLOCS: " ++ pprShowLs allocs
                   let v_grad = fst (allocs !! 0)
                       v_mom0 = fst (allocs !! 1)
                       v_mom = fst (allocs !! 2)
                   s1 <- case Map.lookup v_grad shpCtx of
                           Just (S.BlkOf vs) -> cgXferBlk v_strct vs v_grad
                           _ -> error $ "[CgStInit] | Couldn't find " ++ pprShow v_mom0
                   s2 <- case Map.lookup v_mom0 shpCtx of
                           Just (S.BlkOf vs) -> cgCpyBlk loc v_strct v_mom0 vs
                           _ -> error $ "[CgStInit] | Couldn't find " ++ pprShow v_mom0
                   s3 <- case Map.lookup v_mom shpCtx of
                           Just (S.BlkOf vs) -> cgCpyBlk loc v_strct v_mom vs
                           _ -> error $ "[CgStInit] | Couldn't find " ++ pprShow v_mom
                   return $ C.seqStmt [ s1, s2, s3 ]
            _ -> return $ C.mkSkip
      go (Tensor k1 k2) =
          do s1 <- go k1
             s2 <- go k2
             return $ C.Seq s1 s2

              
{-| [Note] Allocate all state

AUX, CURR, PROP structs

(A) Replicate model state

Suppose model looks like this:

(h_1, .., h_k) => {
  param p_1 .. ;
  ..
  param p_n .. ;
  data d_1 .. ;
  ..
  data d_m .. ;
}

Then copy code looks like this:

AUX.h_1 = xfer(h_1); ..; AUX.h_k = xfer(h_k);

CURR.p_1 = xfer(p_1); ..; CURR.p_n = xfer(p_n);
PROP.p_1 = xfer(p_1); ..; PROP.p_n = xfer(p_n);

CURR.d_1 = xfer(d_1); ..; CURR.d_m = xfer(d_m);
PROP.d_1 = CURR.d_1; ..; PROP.d_m = CURR.d_m;


(B) Create parameter blocks 

Suppose we have blocks: [ p_1, p_2 ] and [ p_3, p_1 ]

Then code looks like this:

CURR.blk_p_1_p_2 = group(&CURR.p_1, &CURR.p_2);
CURR.blk_p_3_p_1 = group(&CURR.p_3, &CURR.p_1);
PROP.blk_p_1_p_2 = group(&PROP.p_1, &PROP.p_2);
PROP.blk_p_3_p_1 = group(&PROP.p_3, &PROP.p_1);


(C) Allocate auxilliary memory

Suppose we have auxilliary g_1:shp_1, .., g_n, shp_n

Then code looks like this:

AUX.g_1 = cgAuxShp(shp_1);
..
AUX.g_n = cgAuxShp(shp_n);

-}         
cgStInit :: InferCtx (TVar Typ) -> S.ShpCtx (TVar Typ) -> C.Exp (TVar C.Typ) -> Kern code (TVar Typ) -> CgM (C.Decl (TVar C.Typ))
cgStInit inferCtx shpCtx loc kern =    
    do vRng <- asks cr_vRng
       vAux <- asks cr_vAux 
       vCurr <- asks cr_vCurr
       vProp <- asks cr_vProp
       let modDecls = ic_modDecls inferCtx           
           xferHypers = map (cgXferToNative loc vAux False) (getModHyperIds modDecls)       
           xferParamsCurr = map (cgXferToNative loc vCurr True) (getModParamIds modDecls)
           xferParamsProp = map (cgXferToNative loc vProp True) (getModParamIds modDecls)
           xferDataCurr = map (cgXferToNative loc vCurr True) (getModDataIds modDecls)
           xferDataProp = map (h vCurr vProp) (getModDataIds modDecls)
           modBlkCtx = Map.toList (ic_modBlkCtx inferCtx)       
       globs <- mapM (\(v, shp) -> cgAuxShp True loc vAux v shp) (Map.toList shpCtx)
       grpParamsCurr <- mapM (\(v, vs) -> cgXferBlk vCurr vs v) modBlkCtx
       grpParamsProp <- mapM (\(v, vs) -> cgXferBlk vProp vs v) modBlkCtx
       kernBlks <- cgBlkKernAux shpCtx loc vAux kern
       -- let chkGlobs = map (\(v, _) -> cgChkNative loc vAux v) (Map.toList shpCtx)
       let -- BEWARE: blocks are added but should not be transfered
           mparams = filter (not . isBlkTy . getType') (getModParamIds modDecls)
           params = map (\v -> (cgId (v { t_idKind = Param }), cgTypXfer (idKind v) (getType' v))) (getModHyperIds modDecls ++ mparams ++ getModDataIds modDecls)
           strcts = [ C.Declare (C.Fwd vAux) (Just (C.addrOf exp_MCMC_AUX))
                    , C.Declare (C.Fwd vCurr) (Just (C.addrOf exp_MCMC_CURR))
                    , C.Declare (C.Fwd vProp) (Just (C.addrOf exp_MCMC_PROP)) ]
           s1 = C.assignStmt' (C.derefStrct vAux vRng) (C.LibCall "augur_rng_setup" [ C.mkInt 0 ])
           s2 = C.mkLibCall "initMcmc" []
           s3 = C.seqStmt (cgKernParamInit (C.Var vAux) kern)
           bodys = [ strcts
                   , xferHypers
                   , xferParamsCurr, xferDataCurr
                   , xferParamsProp, xferDataProp                   
                   , globs
                   , grpParamsCurr, grpParamsProp, [ kernBlks ]
                   , [ s1, s2, s3 ] ]
           body = C.seqStmt (concat bodys)
       return $ C.Fun [] (mkName "augur_iface_init") params body Nothing C.VoidTy
    where
      h vCurr vProp v =
          let v' = cgId v
          in
            C.assignStmt' (C.derefStrct vProp v') (C.derefStrct vCurr v')

             
cgStSetPt :: CompInfo -> InferCtx (TVar Typ) -> CompM (C.Decl (TVar C.Typ))
cgStSetPt cinfo inferCtx =
    do let modDecls = ic_modDecls inferCtx
       v_curr <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local (C.PtrTy ty_AugurMod)
       v_prop <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local (C.PtrTy ty_AugurMod)
       let foo = [ C.assignStmt v_curr (C.addrOf exp_MCMC_CURR)
                 , C.assignStmt v_prop (C.addrOf exp_MCMC_PROP) ]
           mparams = filter (not . isBlkTy . getType') (getModParamIds modDecls)
           params = map (\v -> (cgId (v { t_idKind = Param }), cgTypXfer (idKind v) (getType' v))) mparams                    
           loc = exp_AUGUR_CPU
           scpy1 = map (cgXferToNative loc v_curr True) (getModParamIds modDecls)
           scpy2 = map (cgXferToNative loc v_prop True) (getModParamIds modDecls)
           body = C.seqStmt (foo ++ scpy1 ++ scpy2 )
       return $ C.Fun [] (mkName "augur_set_pt") params body Nothing C.VoidTy


             
cgStCpy :: CompInfo -> InferCtx (TVar Typ) -> CompM (C.Decl (TVar C.Typ))
cgStCpy cinfo inferCtx  =
    do let modDecls = ic_modDecls inferCtx
       v_mod <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local (C.PtrTy ty_AugurMod)
       let foo = [ C.assignStmt v_mod (C.addrOf exp_MCMC_CURR) ]
           mparams = filter (not . isBlkTy . getType') (getModParamIds modDecls)
           params = map (\v -> (cgId (v { t_idKind = Param }), cgTypXfer (idKind v) (getType' v))) mparams                    
           loc = exp_AUGUR_CPU
           scpy = map (cgXferFromNative loc v_mod) (getModParamIds modDecls)
           body = C.seqStmt (foo ++ scpy )
           eLL = projMCMC "currLL"
       return $ C.Fun [] (mkName "augur_cpy") params body (Just eLL) C.DblTy


              
-----------------------------------
-- == Top-level

initCgRdr :: CompInfo -> TVar C.Typ -> CompM CgRdr
initCgRdr cinfo v_rng =
    do let genSym = getGenSym cinfo
       v_aux <- lift $ mkTyIdIO genSym Anon Local (C.PtrTy ty_AugurAux)
       v_curr <- lift $ mkTyIdIO genSym Anon Local (C.PtrTy ty_AugurMod)
       v_prop <- lift $ mkTyIdIO genSym Anon Local (C.PtrTy ty_AugurMod)
       return $ CR v_rng v_aux v_curr v_prop genSym

              
runCgStInit :: CompInfo -> TVar C.Typ -> InferCtx (TVar Typ) -> S.ShpCtx (TVar Typ) -> Kern code (TVar Typ) -> CompM (C.Decl (TVar C.Typ))
runCgStInit cinfo v_rng inferCtx shpCtx kern =
    do rdr <- initCgRdr cinfo v_rng
       let loc = C.mkLiterally enum_AUGUR_CPU
       (v, _, _) <- runRWST (cgStInit inferCtx shpCtx loc kern) rdr ()
       return v


runCgStSetPt :: CompInfo -> InferCtx (TVar Typ) -> CompM (C.Decl (TVar C.Typ))
runCgStSetPt = cgStSetPt
              
runCgStCpy :: CompInfo -> InferCtx (TVar Typ) -> CompM (C.Decl (TVar C.Typ))
runCgStCpy cinfo inferCtx =
    cgStCpy cinfo inferCtx


cgKernParamInit :: C.Exp (TVar C.Typ) -> Kern code (TVar Typ) -> [C.Stmt (TVar C.Typ)]
cgKernParamInit e_strct = go
    where
      go (Base kind _ _ _ kernParams _) =
          case kind of
            GradProp (HMC _ _ simLen stepSize) ->
                [ C.assignStmt' (C.derefStrct' e_strct (C.Var (cgId (kernParams !! 0)))) (C.Lit (C.Dbl simLen))
                , C.assignStmt' (C.derefStrct' e_strct (C.Var (cgId (kernParams !! 1)))) (C.Lit (C.Dbl stepSize)) ]
            _ -> [C.mkSkip]
      go (Tensor k1 k2) = go k1 ++ go k2


runCgCallLLMod :: CompM (C.Decl (TVar C.Typ))
runCgCallLLMod =
    do let params = []
           body = C.mkSkip
           retExp = Just $ C.LibCall "ll_mod" [ exp_MCMC_AUX, exp_MCMC_CURR ]
       return $ C.Fun [] (mkName "augur_get_curr_ll") params body retExp C.DblTy
 
