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

{-# LANGUAGE FlexibleContexts #-}

module Core.CgUserProp
    ( runMwgAllDecls
    , runMwgKern ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Var
import Core.DensSyn
import qualified Core.KernSyn as K
import Comm.DistSyn
import Comm.Prim
import qualified Low.LowSyn as L
import qualified Low.LowXXSyn as LX
import Low.LowpPrimSyn
import Core.CoreSyn
import Core.CoreTySyn
import Compile.CompData
import Core.CgCore
import Core.CgLike
import Low.LowShpSyn as S
    

----------------------------------------------------------------------
-- = CgProp Description
{-| [Note]

p(mu_k | mu0, cov0) ...    ~ MvNormal(mu[k] | mu1, cov1)

llCorrect -> Scalar
prop_mu(idxs) {
  k = idxs[0]
  mu'[k] = MvNormal(mu[k] | mu1, cov1).sample
  llCorrect = MvNormal(mu[k] | mu1, cov1).ll 
                   -  MvNormal(mu'[k] | mu1, cov1).ll
}

swap_mu(dir, idxs) {
  k = idxs[0]
  if (dir == 0) {
    mu[k] = mu'[k]
  }
  else {
    mu'[k] = mu[k]
  }
}

mwg_mu() {
  for k <- gen_k {
    mwg(k);
  }
}

-->

C (Level)

prop_mu(aux, curr, prop, idxs) {
  k = idxs[0]
  mu'[k] = MvNormal(mu[k] | mu1, cov1).sample
  aux.prop_mu_ll = MvNormal(mu[k] | mu1, cov1).ll 
                   -  MvNormal(mu'[k] | mu1, cov1).ll
}

swap_mu(aux, curr, prop, idxs, dir) {
  k = idxs[0]
  if (dir == 0) {
    curr.mu[k] = prop.mu[k]
  }
  else {
    prop.mu[k] = curr.mu[k]
  }
}

mwg_mu(aux, curr, prop) {
  for k <- gen_k {
    idxs = { k }
    mwg(aux, curr, prop, idxs, aux.prop_mu_ll, prop_mu, swap_mu, fcLL)
  }
}

-}


-----------------------------------
-- == Types and operations
    
type PropM b = ReaderT (PropRdr b) CompM

data PropRdr b =
    PR { pr_prop :: Name
       , pr_swap :: Name
       , pr_like :: Name
       , pr_vIdxs :: b
       , pr_vLLCorrect :: b
       , pr_vDir :: b
       , pr_propVarsM :: Map.Map b b }

    
projIdxs :: b -> [b] -> [L.Stmt b]
projIdxs vIdxs idxs =
    map (\(v, i) -> L.Assign v (L.Proj (L.Var vIdxs) [L.Lit (L.Int i)])) (zip idxs [0..])


        
-----------------------------------
-- == Transformation

propBody :: (TypedVar b Typ) => Fn b -> PropM b (L.Stmt b)
propBody (Dens dist pt es) =
    do propVarsM <- asks pr_propVarsM
       vLLCorrect <- asks pr_vLLCorrect
       let v_prop = case Map.lookup (densPtVar pt) propVarsM of
                      Just v -> v 
                      Nothing -> error $ "[CgProp] | Lookup of " ++ pprShow (densPtVar pt) ++ " failed in context: " ++ pprShow propVarsM                              
           pt' = mkDensPt v_prop (densPtIdx' pt)
           e_rhs = L.DistOp Sample DM_Fn dist (map cgExp es)
           s_prop = L.Store v_prop (map cgExp (densPtIdx' pt)) L.Update e_rhs
           llArgs = [ L.DistOp LL DM_Fn dist (map cgExp (pt : es))
                    , L.DistOp LL DM_Fn dist (map cgExp (pt' : es)) ]
           s_ll = L.Store vLLCorrect [] L.Update (L.Call (L.PrimId DM_Fn PM_Fn Minus) llArgs)       
       return $ L.seqStmt [ s_prop, s_ll ]
propBody (Ind _ _) = error $ "[CgProp] | Shouldn't happen"
propBody (Let x e fn) =
    do s <- propBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
propBody (Prod _ _) = error $ "[CgProp] | Shouldn't happen"
propBody (Pi x gen fn) =
    do let gen' = cgGen gen
       s <- propBody fn
       return $ L.Loop L.Sequential x gen' s


propDecl :: (TypedVar b Typ) => Fn b -> PropM b (L.Decl b)
propDecl fn =
    do name <- asks pr_prop
       body <- propBody fn
       v_idxs <- asks pr_vIdxs
       v_llCorrect <- asks pr_vLLCorrect
       let params = [ (v_idxs, getType' v_idxs) ]
           allocs = [ v_llCorrect ]
           idxs = densPtIdx (gatherDensPt fn)
           body' = L.seqStmt (body : projIdxs v_idxs idxs)
       return $ L.Fun name params allocs body' Nothing UnitTy
              
              
swapBody :: (TypedVar b Typ) => Fn b -> PropM b (L.Stmt b)
swapBody (Dens _ pt _) =
    do propVarsM <- asks pr_propVarsM
       v_dir <- asks pr_vDir
       let v_mod = densPtVar pt
           v_mod' = case Map.lookup (densPtVar pt) propVarsM of
                      Just v -> v
                      Nothing -> error $ "[CgProp] | Lookup of " ++ pprShow (densPtVar pt) ++ " failed in context: " ++ pprShow propVarsM
           pt' = mkDensPt v_mod' (densPtIdx' pt)
           idxs = map cgExp (densPtIdx' pt)
           s1 = L.Store v_mod idxs L.Update (cgExp pt')
           s2 = L.Store v_mod' idxs L.Update (cgExp pt)
           e_cond = L.Call (L.PrimId DM_Fn PM_Fn EqEq) [ L.Var v_dir, L.Lit (L.Int 0) ]
       return $ L.If e_cond s1 s2
swapBody (Ind _ _) = error $ "[CgProp] | Shouldn't happen"
swapBody (Let x e fn) =
    do s <- swapBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
swapBody (Prod _ _) = error $ "[CgProp] | Shouldn't happen"
swapBody (Pi _ _ fn) = swapBody fn

                       
swapDecl :: (TypedVar b Typ) => Fn b -> PropM b (L.Decl b)
swapDecl fn =
    do name <- asks pr_swap
       body <- swapBody fn
       v_dir <- asks pr_vDir
       v_idxs <- asks pr_vIdxs
       let params = [ (v_idxs, getType' v_idxs), (v_dir, getType' v_dir) ]
           allocs = []
           idxs = densPtIdx (gatherDensPt fn)
           body' = L.seqStmt (projIdxs v_idxs idxs ++ [ body ])
       return $ L.Fun name params allocs body' Nothing UnitTy

                       
mwgBody :: (TypedVar b Typ) => Fn b -> PropM b (L.Stmt b)
mwgBody (Dens _ pt _) =
    do v_llCorrect <- asks pr_vLLCorrect
       propVarsM <- asks pr_propVarsM
       prop <- asks pr_prop
       swap <- asks pr_swap
       like <- asks pr_like
       let pt' = case Map.lookup (densPtVar pt) propVarsM of
                   Just v -> mkDensPt v (densPtIdx' pt)
                   Nothing -> error $ "[CgProp] | Lookup of " ++ pprShow (densPtVar pt) ++ " failed in context: " ++ pprShow propVarsM
           args = [ cgExp pt, cgExp pt', L.Var v_llCorrect ] ++ map cgExp (densPtIdx' pt)
       return $ L.Exp (L.Call (L.PrimId DM_Fn PM_Fn (MWG prop swap like)) args)
mwgBody (Ind _ _) = error $ "[CgProp] | Shouldn't happen"
mwgBody (Let x e fn) =
    do s <- mwgBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
mwgBody (Prod _ _) = error $ "[CgProp] | Shouldn't happen"
mwgBody (Pi x gen fn) =
    do let gen' = cgGen gen
       s <- mwgBody fn
       return $ L.Loop L.Sequential x gen' s

              
mwgDecl :: (TypedVar b Typ) => b -> Fn b -> PropM b (L.Decl b)
mwgDecl v_mod fn =
    do v_llCorrect <- asks pr_vLLCorrect
       body <- mwgBody fn
       let name = mkCompName "mwg" (varName v_mod)
           params = []
           allocs = [ v_llCorrect ]
       return $ L.Fun name params allocs body Nothing UnitTy
              
              
mwgAllDecls :: (TypedVar b Typ) => b -> Fn b -> PropM b (LX.LowPP b, LX.LowPP b, LX.LowPP b)
mwgAllDecls v_mod fn =
    do v_llCorrect <- asks pr_vLLCorrect
       prop <- propDecl fn
       swap <- swapDecl fn
       mwg <- mwgDecl v_mod fn
       let prop' = LX.LowPP (LX.LowXX (Map.singleton v_llCorrect S.Scalar) True prop)
           swap' = LX.LowPP (LX.LowXX Map.empty True swap)
           mwg' = LX.LowPP (LX.LowXX (Map.singleton v_llCorrect S.Scalar) True mwg)
       return (prop', swap', mwg')


              
-----------------------------------
-- == Top-level

runMwgAllDecls :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> Fn b -> CompM (LX.LowPP b, LX.LowPP b, LX.LowPP b, LX.LowPP b)
runMwgAllDecls cinfo copt inferCtx v_mod fcFn propFn =
    do v_idxs <- lift $ mkTyIdIO (getGenSym cinfo) (mkName "idxs") Param (VecTy IntTy)
       v_llCorrect <- lift $ mkTyIdIO (getGenSym cinfo) Anon ModAux RealTy
       v_dir <- lift $ mkTyIdIO (getGenSym cinfo) (mkName "dir") Param IntTy
       likeOne <- runLLFnOne cinfo copt inferCtx v_mod fcFn
       let likeOne' = LX.LowPP (LX.LowXX Map.empty False likeOne)
           propName = mkCompName "prop" (varName v_mod)
           swapName = mkCompName "swap" (varName v_mod)
           likeName = (L.declName likeOne)
           rdr = PR propName swapName likeName v_idxs v_llCorrect v_dir (ic_dupCtx inferCtx)
       (prop', swap', mwg') <- runReaderT (mwgAllDecls v_mod propFn) rdr
       return (prop', swap', likeOne', mwg')

              
-- TODO: Mising like (but never used)
runMwgKern :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> Fn b -> CompM (K.Kern (LX.LowPP b) b)
runMwgKern cinfo copt inferCtx v_mod fcFn propFn =
    do (prop, swap, like, mwg) <- runMwgAllDecls cinfo copt inferCtx v_mod fcFn propFn
       let kind = K.UserProp (K.MWG prop swap like mwg)
           like' = error "[Core.CgUserProp] @runMwgKern | Do not deref" -- TODO: HACK
           kern = K.Base kind (K.Single v_mod) fcFn [] [] like'
       return kern


