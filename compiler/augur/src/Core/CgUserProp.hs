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
       , pr_vLLCorrect :: b
       , pr_vDir :: b
       , pr_propVarsM :: Map.Map b b }

        
-----------------------------------
-- == Transformation

getDensPtPropVar :: (TypedVar b Typ) => Exp b -> PropM b b
getDensPtPropVar pt = 
    do propVarsM <- asks pr_propVarsM
       case Map.lookup (densPtVar pt) propVarsM of
         Just v -> return v
         Nothing -> error $ "[Core.CgUserProp] | Lookup of " ++ pprShow (densPtVar pt) ++ " failed in context: " ++ pprShow propVarsM


propBody :: (TypedVar b Typ) => Fn b -> PropM b (L.Stmt b)
propBody (Dens dist pt es) =
    do vLLCorrect <- asks pr_vLLCorrect
       v_prop <- getDensPtPropVar pt
       let pt' = mkDensPt v_prop (densPtIdx' pt)
           e_rhs = L.DistOp Sample DM_Fn dist (map cgExp es)
           s_prop = L.Store v_prop (map cgExp (densPtIdx' pt)) L.Update e_rhs
           llArgs = [ L.DistOp LL DM_Fn dist (map cgExp (pt : es))
                    , L.DistOp LL DM_Fn dist (map cgExp (pt' : es)) ]
           s_ll = L.Store vLLCorrect [] L.Update (L.Call (L.PrimId DM_Fn PM_Fn Minus) llArgs)
       return $ L.seqStmt [ s_prop, s_ll ]
propBody (Ind _ _) = error $ "[Core.CgUserProp] | Shouldn't happen"
propBody (Let x e fn) =
    do s <- propBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
propBody (Prod _ _) = error $ "[Core.CgUserProp] | Shouldn't happen"
propBody (Pi _ _ fn) = propBody fn
    

propDecl :: (TypedVar b Typ) => Fn b -> PropM b (L.Decl b)
propDecl fn =
    do name <- asks pr_prop
       body <- propBody fn
       v_llCorrect <- asks pr_vLLCorrect
       let idxs = densPtIdx (gatherDensPt fn)
           params = map (\idx -> (setIdKind idx GridIdx, IntTy)) idxs
           allocs = [ v_llCorrect ]
       return $ L.Fun name params allocs body Nothing UnitTy
              
              
swapBody :: (TypedVar b Typ) => Fn b -> PropM b (L.Stmt b)
swapBody (Dens _ pt _) =
    do v_dir <- asks pr_vDir
       let v_mod = densPtVar pt
       v_mod' <- getDensPtPropVar pt
       let pt' = mkDensPt v_mod' (densPtIdx' pt)
           idxs = map cgExp (densPtIdx' pt)
           s1 = L.Store v_mod idxs L.Update (cgExp pt')
           s2 = L.Store v_mod' idxs L.Update (cgExp pt)
           e_cond = L.Call (L.PrimId DM_Fn PM_Fn EqEq) [ L.Var v_dir, L.Lit (L.Int 0) ]
       return $ L.If e_cond s1 s2
swapBody (Ind _ _) = error $ "[Core.CgUserProp] | Shouldn't happen"
swapBody (Let x e fn) =
    do s <- swapBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
swapBody (Prod _ _) = error $ "[Core.CgUserProp] | Shouldn't happen"
swapBody (Pi _ _ fn) = swapBody fn

                       
swapDecl :: (TypedVar b Typ) => Fn b -> PropM b (L.Decl b)
swapDecl fn =
    do name <- asks pr_swap
       body <- swapBody fn
       v_dir <- asks pr_vDir
       let idxs = densPtIdx (gatherDensPt fn)
           params = (v_dir, getType' v_dir) : map (\idx -> (setIdKind idx GridIdx, IntTy)) idxs
           allocs = []
       return $ L.Fun name params allocs body Nothing UnitTy

                       
mwgBody :: (TypedVar b Typ) => Fn b -> PropM b (L.Stmt b)
mwgBody (Dens _ pt _) =
    do v_llCorrect <- asks pr_vLLCorrect
       -- propVarsM <- asks pr_propVarsM
       prop <- asks pr_prop
       swap <- asks pr_swap
       like <- asks pr_like
       v_mod' <- getDensPtPropVar pt
       let pt' = mkDensPt v_mod' (densPtIdx' pt)           
           args = [ cgExp pt, cgExp pt', L.Var v_llCorrect ] ++ map cgExp (densPtIdx' pt)
       return $ L.Exp (L.Call (L.PrimId DM_Fn PM_Fn (MWG prop swap like)) args)
mwgBody (Ind _ _) = error $ "[Core.CgUserProp] | Shouldn't happen"
mwgBody (Let x e fn) =
    do s <- mwgBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
mwgBody (Prod _ _) = error $ "[Core.CgUserProp] | Shouldn't happen"
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
       let idxs = densPtIdx (gatherDensPt fn)
           prop' = LX.LowPP (LX.LowXX (Map.singleton v_llCorrect S.Scalar) True (LX.DevCall False) idxs prop)
           swap' = LX.LowPP (LX.LowXX Map.empty True (LX.DevCall False) idxs swap)
           mwg' = LX.LowPP (LX.LowXX (Map.singleton v_llCorrect S.Scalar) True (LX.HostCall False) [] mwg)
       return (prop', swap', mwg')


              
-----------------------------------
-- == Top-level

runMwgAllDecls :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> Fn b -> CompM (LX.LowPP b, LX.LowPP b, LX.LowPP b, LX.LowPP b)
runMwgAllDecls cinfo copt inferCtx v_mod fcFn propFn =
    do v_llCorrect <- lift $ mkTyIdIO (getGenSym cinfo) Anon ModAux RealTy
       v_dir <- lift $ mkTyIdIO (getGenSym cinfo) (mkName "dir") Param IntTy
       (s_projIdx, likeOne) <- runLLFnOne cinfo copt inferCtx v_mod fcFn
       let likeOne' = LX.LowPP (LX.LowXX Map.empty False (LX.DevCall True) s_projIdx likeOne)
           propName = mkCompName "prop" (varName v_mod)
           swapName = mkCompName "swap" (varName v_mod)
           likeName = (L.declName likeOne)
           rdr = PR propName swapName likeName v_llCorrect v_dir (ic_dupCtx inferCtx)
       (prop', swap', mwg') <- runReaderT (mwgAllDecls v_mod propFn) rdr
       return (prop', swap', likeOne', mwg')

              
runMwgKern :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> Fn b -> CompM (K.Kern (LX.LowPP b) b)
runMwgKern cinfo copt inferCtx v_mod fcFn propFn =
    do (prop, swap, like, mwg) <- runMwgAllDecls cinfo copt inferCtx v_mod fcFn propFn
       let kind = K.UserProp (K.MWG prop swap like mwg)
           like' = error "[Core.CgUserProp] @runMwgKern | Do not deref"
           kern = K.Base kind (K.Single v_mod) fcFn [] [] like'
       return kern
