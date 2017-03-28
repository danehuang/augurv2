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

module Core.CgDisc where

import Control.Monad.RWS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Pretty
import AstUtil.Var
import Core.DensSyn
import qualified Core.RwCore as RW
import Comm.DistSyn
import Comm.Prim
import qualified Low.LowSyn as L
import qualified Low.LowXXSyn as LX
import Low.LowpPrimSyn
import Core.CoreSyn
import qualified Core.KernSyn as K
import Core.CgCore
import Core.CoreTySyn
import qualified Low.LintLow as Lint
import Compile.CompData


----------------------------------------------------------------------
-- = CgDisc Description
{-| [Note]

Approximated closed-form full-conditional for Gibbs sampling.

Separate out the full-conditional into the updating + rest parts.

-}


    
-----------------------------------
-- == Types and operations

type TyId = TVar Typ
type DiscM = RWST DiscRdr [()] DiscSt CompM
data DiscRdr =
    DiscRdr { dr_v_pmf :: TyId
            , dr_v_prob :: TyId
            , dr_v_norm :: TyId
            , dr_genSym :: GenSym }
data DiscSt =
    DiscSt { ds_seen :: Set.Set TyId
           , ds_ctx :: [TyId] }

freshId :: Name -> IdKind -> Typ -> DiscM TyId
freshId n ik ty =
    do genSym <- asks dr_genSym
       lift $ lift $ mkTyIdIO genSym n ik ty
    
-----------------------------------
-- == Transformation



discFnBody :: Fn TyId -> DiscM (L.Stmt TyId)
discFnBody (Dens dist pt es) =
    do v_prob <- asks dr_v_prob
       let es' = map cgExp (pt : es)
           s_prob = L.Store v_prob [] L.MInc (L.DistOp Pdf DM_Fn dist es')
       return s_prob
discFnBody (Ind fn conds) =
    do let conds' = map cgIndCond conds
       s <- discFnBody fn
       return $ L.If (andExp conds') s L.Skip
discFnBody (Let x e fn) =
    do let s1 = L.Assign x (cgExp e)
       s2 <- discFnBody fn
       return $ L.seqStmt [ s1, s2 ]
discFnBody (Prod fn1 fn2) =
    do s1 <- discFnBody fn1
       s2 <- discFnBody fn2
       return $ L.seqStmt [ s1, s2 ]
discFnBody (Pi x gen fn) =
    do seen <- gets ds_seen
       if x `Set.member` seen
       then discFnBody fn
       else do modify (\s -> s { ds_seen = Set.insert x seen })
               s <- discFnBody fn
               return $ L.Loop L.Parallel x (cgGen gen) s 


allocPmf :: [Exp TyId] -> Dist -> L.Exp TyId
allocPmf es = f
    where
      f Bernoulli =
          L.Call (L.PrimId DM_Fn PM_Fn AllocVecFromShape) [2]
      f Categorical =
          L.Call (L.PrimId DM_Fn PM_Fn ReadVecFromShape) [cgExp (es !! 0)] 
      f Geometric =
          -- TODO: Use Markov's inequality, but need to dynamically resize
          L.Call (L.PrimId DM_Fn PM_Fn AllocVecFromShape) [ 100 ] 
      f Poisson =
          -- TODO: Use Markov's inequality, but need to dynamically resize
          L.Call (L.PrimId DM_Fn PM_Fn AllocVecFromShape) [ 100 ] 
      f dist = error $ "[CgDisc] @allocPmf | Shouldn't happen: " ++ pprShow dist


-- TODO: compute normalization const
discFnMarg :: Fn TyId -> Fn TyId -> DiscM (L.Stmt TyId)
discFnMarg fnPt fnRest = f fnPt
    where
      f (Dens dist pt es) =
          do v_pmf <- asks dr_v_pmf                  -- pmf array
             v_prob <- asks dr_v_prob                -- probability accumulator
             v_norm <- asks dr_v_norm                -- normalization constant
             v_marg <- freshId Anon Local IntTy      -- marginalizing variable
             let ept = pt
                 es' = map cgExp (Var v_marg : es)
                 s0 = L.Assign v_prob (L.Lit (L.Real 1))
                 s1 = L.Store v_prob [] L.MInc (L.DistOp Pdf DM_Fn dist es')
                 -- s1 = L.Assign v_prob (L.Var v_prob * L.DistOp Pdf DM_Fn dist es')
             s2 <- discFnBody (substAExpFn (Var v_marg) ept fnRest)
             let s_pmf = L.Assign v_pmf (allocPmf es dist)
                 margGen = L.Until (L.Lit (L.Int 0)) (L.Call (L.PrimId DM_Fn PM_Fn SizeVec) [ L.Var v_pmf ])
                 s3 = L.Store v_pmf [ L.Var v_marg ] L.Update (L.Var v_prob)
                 s_marg = L.Loop L.Parallel v_marg margGen (L.seqStmt [ s0, s1, s2, s3 ])
                 s_samp = case ept of
                            Var x -> L.Store x [] L.Update (L.Call (L.PrimId DM_Fn PM_Fn NormAndSamp) [ L.Var v_pmf])
                            Proj (Var x) es -> L.Store x (map cgExp es) L.Update (L.Call (L.PrimId DM_Fn PM_Fn NormAndSamp) [ L.Var v_pmf])
                            _ -> error $ "Huh!?"
             return $ L.seqStmt [ s_pmf, s_marg, s_samp ]
      f (Ind fn cond) =
          error $ "[CgDisc] @discFnMarg | Shouldn't happen: " ++ pprShow (Ind fn cond)
      f (Let x e fn) =
          do let s1 = L.Assign x (cgExp e)
             s2 <- f fn
             return $ L.seqStmt [ s1, s2 ]
      f (Prod fn1 fn2) =
          error $ "[CgDisc] @discFnMarg | Shouldn't happen: " ++ pprShow (Prod fn1 fn2)
      f (Pi x gen fn) =
          do modify (\st -> st { ds_seen = Set.insert x (ds_seen st) })
             s <- f fn
             return $ L.Loop L.Parallel x (cgGen gen) s 
              

              
discFn :: TyId -> Fn TyId -> Fn TyId -> DiscM (L.Decl TyId)
discFn v_mod fnPt fnRest = 
    do body <- discFnMarg fnPt fnRest
       return $ L.Fun (mkCompName "disc" (varName v_mod)) [] [] body Nothing UnitTy


-----------------------------------
-- == Top-level

runDiscFn :: CompInfo -> CompOpt -> InferCtx TyId -> TyId -> Fn TyId -> CompM (K.Kern (LX.LowPP TyId) TyId)
runDiscFn cinfo copt inferCtx v_mod fn =
    do v_pmf <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local (VecTy RealTy)
       v_prob <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local RealTy
       v_norm <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local RealTy
       let discRdr = DiscRdr v_pmf v_prob v_norm (getGenSym cinfo)
           discSt = DiscSt Set.empty []
           (fn', fns') = RW.split v_mod fn
       (v, _, _) <- runRWST (discFn v_mod fn' (prodFn fns')) discRdr discSt
       samp <- runLint copt v (Lint.runLintDecl cinfo False inferCtx)
       let like' = samp -- TODO: HACK? but never used
           samp' = LX.LowPP (LX.LowXX Map.empty False (LX.HostCall False) [] samp)
           kind = K.Gibbs (K.Disc samp')
           like'' = error $ "[Core.CgDisc] | Shouldn't dereference" -- LX.LowPP (LX.LowXX Map.empty False (LX.HostCall False) like') -- TODO: hack, but never used
           kern = K.Base kind (K.Single v_mod) fn [] [] like''
       return $ kern
       
