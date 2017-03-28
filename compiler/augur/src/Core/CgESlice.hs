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

module Core.CgESlice(runESliceFn) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace

import AstUtil.Pretty
import AstUtil.Var
import Core.DensSyn
import Comm.Prim
import qualified Low.LowSyn as L
import qualified Low.LowXXSyn as LX
import qualified Core.KernSyn as K
import Low.LowpPrimSyn
import Core.CoreSyn
import Core.CoreTySyn
import Compile.CompData
import Core.CgCore
import Core.RwCore as RW
import Core.CgLike
import Low.LowShpSyn as S


----------------------------------------------------------------------
-- = CgESlice Description
{-| [Note]

p(mu_k | mu0, cov0) Pi_n p(y_n | mu_{z_n})

workCov -> DblConn (Max mu 1) (Max mu 1) Scalar
workNu -> Max mu 1
eslice_prop() {
for k <- gen_k
  elliptical_slice(objFn)(mu[k], mu'[k], mu0, cov0, workCov, workNu, k)
}

-->

C (Level)

eslice_prop(aux, curr, prop) {
for k <- gen_k
  idxs = { k }
  elliptical_slice(aux, curr, prop, curr.mu[k], prop.mu[k], mu0, cov0, workCov, workNu, idxs, objFn)
}


-}


-----------------------------------
-- == Types and operations

type ESliceM b = ReaderT (ESliceRdr b) CompM

data ESliceRdr b =
    ER { er_like :: Name
       , er_propVarsM :: Map.Map b b
       , er_vWorkCov :: b
       , er_vWorkNu :: b }

    
-----------------------------------
-- == Transformation

getDensPtPropVar :: (TypedVar b Typ) => Exp b -> ESliceM b b
getDensPtPropVar pt = 
    do propVarsM <- asks er_propVarsM
       case Map.lookup (densPtVar pt) propVarsM of
         Just v -> return v
         Nothing -> error $ "[Core.CgESlice] | Lookup of " ++ pprShow (densPtVar pt) ++ " failed in context: " ++ pprShow propVarsM


esliceBody :: (TypedVar b Typ) => Fn b -> ESliceM b (L.Stmt b)
esliceBody (Dens _ pt es) =
    do like <- asks er_like
       v_workCov <- asks er_vWorkCov
       v_workNu <- asks er_vWorkNu
       v_mod <- getDensPtPropVar pt
       let pt' = mkDensPt v_mod (densPtIdx' pt)
           args = [ cgExp pt, cgExp pt' ] ++ map cgExp es ++ 
                  [ L.Var v_workCov, L.Var v_workNu ] ++ map cgExp (densPtIdx' pt)
       return $ L.Exp (L.Call (L.PrimId DM_Fn PM_Fn (EllipSlice like)) args)
esliceBody (Ind _ _) = error $ "[Core.CgESlice] | Shouldn't happen"
esliceBody (Let x e fn) =
    do s <- esliceBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
esliceBody (Prod _ _) = error $ "[Core.CgESlice] | Shouldn't happen"
esliceBody (Pi x gen fn) =
    do let gen' = cgGen gen
       s <- esliceBody fn
       return $ L.Loop L.Sequential x gen' s

              
esliceFn :: (TypedVar b Typ) => b -> Fn b -> ESliceM b (L.Decl b)
esliceFn v_mod fn =
    do body <- esliceBody fn
       v_workCov <- asks er_vWorkCov
       v_workNu <- asks er_vWorkNu
       let name = mkCompName "eslice" (varName v_mod)
           params = []
           allocs = [ v_workCov, v_workNu ]
       return $ L.Fun name params allocs body Nothing UnitTy

              
-----------------------------------
-- == Top-level

runESliceFn :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> CompM (K.Kern (LX.LowPP b) b)
runESliceFn cinfo copt inferCtx v_mod fn =
    do (projIdx, likeOne) <- runLLFnOne cinfo copt inferCtx v_mod fn
       let (hdFn, _) = RW.split v_mod fn
       v_workCov <- lift $ mkTyIdIO (getGenSym cinfo) Anon ModAux (MatTy RealTy)
       v_workNu <- lift $ mkTyIdIO (getGenSym cinfo) Anon ModAux (VecTy RealTy)
       let pt = gatherDensPt hdFn
           se = S.MaxDim (densPtVar pt) (length (densPtIdx pt))
           shpCtx = Map.fromList [ (v_workCov, S.MatConn se se S.Scalar)
                                 , (v_workNu, S.SingConn se S.Scalar) ]
       eslice <- runReaderT (esliceFn v_mod hdFn) (ER (L.declName likeOne) (ic_dupCtx inferCtx) v_workCov v_workNu)
       let likeOne' = LX.LowPP (LX.LowXX Map.empty False (LX.DevCall True) projIdx likeOne)
           eslice' = LX.LowPP (LX.LowXX shpCtx True (LX.HostCall False) [] eslice)
           kind = K.Slice (K.Ellip likeOne' eslice')
           like' = error $ "[Core.CgESlice] @runESliceFn | Shouldn't deref"
           kern = K.Base kind (K.Single v_mod) fn [] [] like' 
       return kern
