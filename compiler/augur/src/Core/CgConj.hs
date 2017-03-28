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

module Core.CgConj
    ( chkConj, chkConjM
    , runConjFn
    ) where

import Control.Monad.RWS
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Pretty
import AstUtil.Var
import AstUtil.VarOp
import AstUtil.AlphaEquiv
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
import qualified Low.LowShpSyn as S
    
    
----------------------------------------------------------------------
-- = CgConj Description
{-| [Note]

Contains code generators for conjugacy relations.

1) Conjugacy specific code, i.e., compute statistics.
2) Sampling code.

-}


-----------------------------------
-- == Types and operations

type TyId = TVar Typ
type ConjM = RWST ConjRdr [()] ConjSt CompM

data ConjRdr = CR { cr_genSym :: GenSym }

data ConjSt = CS { cs_seen :: Set.Set TyId
                 , cs_stats :: [(TyId, S.Shp TyId)] }

data DensInfo = DI { di_dist :: Dist
                   , di_pt :: Exp TyId
                   , di_args :: [Exp TyId] }


type SampM = RWST SampRdr [()] () CompM
data SampRdr = SR { sr_l_dist :: Dist
                  , sr_idxs :: [TyId]
                  , sr_l_fn :: Fn TyId
                  , sr_stats :: [TyId] }
              

conjStatId :: Name -> Typ -> ConjM TyId
conjStatId name ty =
    do genSym <- asks cr_genSym
       v <- lift $ lift $ mkTyIdIO genSym name ModAux ty
       return v

              
withIdx :: TyId -> SampM a -> SampM a
withIdx x = local (\rdr -> rdr { sr_idxs = x : sr_idxs rdr })

            
            
-----------------------------------
-- == Conjugacy helper code

conjStatLike :: DensInfo -> (DensInfo -> DensInfo -> ConjM (L.Stmt TyId)) -> Fn TyId -> ConjM (L.Stmt TyId)
conjStatLike di comp = go
    where
      go (Dens dist pt es) =
          comp di (DI dist pt es)
      go (Ind fn conds) =
          do s <- go fn
             return $ foldl (\acc cond -> case cond of
                                            CatCond x e -> subst x (cgExp e) acc
                            ) s conds
      go (Let x e fn) =
          do let s1 = L.Assign x (cgExp e)
             s2 <- go fn
             return $ L.seqStmt [ s1, s2 ]
      go (Prod fn1 fn2) =
          do s1 <- go fn1
             s2 <- go fn2
             return $ L.seqStmt [ s1, s2 ]
      go (Pi x gen fn) =
          do seen <- gets cs_seen
             if x `Set.member` seen
             then go fn
             else do modify (\s -> s { cs_seen = Set.insert x seen })
                     s <- go fn
                     return $ L.Loop L.AtomicPar x (cgGen gen) s


conjStatPrior :: (DensInfo -> DensInfo -> ConjM (L.Stmt TyId)) -> Fn TyId -> Fn TyId -> ConjM (L.Stmt TyId)
conjStatPrior comp fnPrior fnLike = go fnPrior
    where
      go (Dens dist ept es) =
          conjStatLike (DI dist ept es) comp fnLike          
      go (Ind _ _) = error $ "[CgConj] | Shouldn't find indicator in prior"
{-
          do let conds' = map cgIndCond conds
             s <- go fn
             return $ L.If (andExp conds') s L.Skip 
-}
      go (Let x e fn) =
          do let s1 = L.Assign x (cgExp e)
             s2 <- go fn
             return $ L.seqStmt [ s1, s2 ]
      go (Prod fn1 fn2) =
          do s1 <- go fn1
             s2 <- go fn2
             return $ L.seqStmt [ s1, s2 ]
      go (Pi x gen fn) =
          do seen <- gets cs_seen
             if x `Set.member` seen
             then go fn
             else do modify (\s -> s { cs_seen = Set.insert x seen })
                     s <- go fn
                     return $ L.Loop L.AtomicPar x (cgGen gen) s 

                            
conjStat :: TyId -> (DensInfo -> DensInfo -> ConjM (L.Stmt TyId)) -> Fn TyId -> Fn TyId -> ConjM (S.ShpCtx TyId, L.Decl TyId)
conjStat v_mod cg p_fn l_fn =
    do let catIdxs = map (\cond -> case cond of
                                     CatCond x _ -> x
                         ) (gatherCatCstr l_fn)
       modify (\st -> st { cs_seen = Set.union (Set.fromList catIdxs) (cs_seen st) })
       body <- conjStatPrior cg p_fn l_fn
       stats <- gets cs_stats
       return ( Map.fromList stats
              , L.Fun (mkCompName "conj_stat" (varName v_mod)) [] (map fst stats) body Nothing UnitTy )


              
-----------------------------------
-- == Distribution-specific conjugacy code

conjDirCat :: DensInfo -> DensInfo -> ConjM (L.Stmt TyId)
conjDirCat (DI Dirichlet p_ept p_es) (DI Categorical l_ept l_es) =
    do v_cnt <- conjStatId Anon (cntTy (densPtTy p_ept))
       let shp_cnt = S.mkCpy (densPtVar p_ept)
           e_cat = cgExp l_ept
       modify (\st -> st { cs_stats = [ (v_cnt, shp_cnt) ] })
       return $ L.Store v_cnt (map cgExp (densPtIdx' p_ept) ++ [e_cat]) L.AtmInc 1
    where
      cntTy (VecTy RealTy) = VecTy IntTy
      cntTy (VecTy ty) = VecTy (cntTy ty)
      cntTy ty = error $ "[CgConj] @conjDirCat | Shouldn't happen " ++ pprShow ty
conjDirCat _ _ =
    error $ "[CgConj] @conjDirCat | Shouldn't happen."

          
conjMvNormMvNorm :: DensInfo -> DensInfo -> ConjM (L.Stmt TyId)
conjMvNormMvNorm (DI MvNormal p_ept p_es) (DI MvNormal l_ept l_es) =
    do v_sum <- conjStatId Anon (sumTy (densPtTy l_ept))
       v_cnt <- conjStatId Anon (cntTy (densPtTy p_ept))
       let shp_sum = S.mkCpy (densPtVar p_ept)
           shp_cnt = S.shpExpsToShp (init (S.tyToDims (densPtVar p_ept)))
       modify (\st -> st { cs_stats = [ (v_sum, shp_sum), (v_cnt, shp_cnt) ] })
       let e_sum = cgExp (mkDensPt v_sum (densPtIdx' p_ept))
           s1 = L.Exp (L.Call (L.PrimId DM_Fn PM_Fn AtmIncVec) [ e_sum, cgExp l_ept ])
           s2 = L.Store v_cnt (map cgExp (densPtIdx' p_ept)) L.AtmInc 1
       return $ L.seqStmt [ s1, s2 ]
    where
      sumTy (VecTy RealTy) = VecTy RealTy
      sumTy (VecTy ty) = VecTy (sumTy ty)
      sumTy ty = error $ "[CgConj] @conjMvNormMvNorm | Shouldn't happen " ++ pprShow ty
      
      cntTy (VecTy RealTy) = IntTy
      cntTy (VecTy ty) = VecTy (cntTy ty)
      cntTy ty = error $ "[CgConj] @conjMvNormMvNorm | Shouldn't happen " ++ pprShow ty
conjMvNormMvNorm _ _ =
    error $ "[CgConj] @conjMvNormMvNorm | Shouldn't happen."
              
          
conjInvWishMvNorm :: DensInfo -> DensInfo -> ConjM (L.Stmt TyId)
conjInvWishMvNorm (DI InvWishart p_ept p_es) (DI MvNormal l_ept l_es) =
    do v_obs <- conjStatId Anon (obsTy (densPtTy p_ept))
       v_cnt <- conjStatId Anon (cntTy (densPtTy p_ept))
       v_diff <- conjStatId Anon (diffTy (densPtTy l_ept))
       let shp_obs = S.shpExpsToShp (S.repShpFromSkel (densPtVar p_ept) (length (densPtIdx p_ept)) (S.Cpy (densPtVar p_ept)))
           shp_cnt = S.shpExpsToShp (init (S.tyToDims (densPtVar p_ept)))
           -- shp_diff = S.shpExpsToShp (S.tyToDims (densPtVar l_ept))
           shp_diff = S.mkCpy (densPtVar l_ept)
       modify (\st -> st { cs_stats = [ (v_obs, shp_obs), (v_cnt, shp_cnt), (v_diff, shp_diff) ] })
       let es_priorIdxs = densPtIdx' p_ept           
           es4 = densPtIdx' l_ept
           e_ci' = cgExp (mkDensPt v_obs es_priorIdxs)
           e_diff' = cgExp (mkDensPt v_diff es4)
           e_lpt' = cgExp l_ept
           e_mean' = cgExp (head l_es)
           s1 = L.Store v_cnt (map cgExp es_priorIdxs) L.AtmInc 1
           -- Warning: Non-functional vector subtraction
           ce2 = (L.PrimId DM_Mem PM_Fn MinusVec) 
           s2 = L.Exp $ L.Call ce2 [ e_diff', e_lpt', e_mean' ]
           ce3 = (L.PrimId DM_Mem PM_Fn AtmIncMatVTMT)
           s3 = L.Exp $ L.Call ce3 [ e_ci', e_diff' ] 
       return $ L.seqStmt [ s1, s2, s3 ]
    where
      obsTy (MatTy RealTy) = MatTy RealTy
      obsTy (VecTy ty) = VecTy (obsTy ty)
      obsTy ty = error $ "[CgConj] @conjInvWishMvNorm 1 | Shouldn't happen " ++ pprShow ty
      
      cntTy (MatTy RealTy) = IntTy
      cntTy (VecTy ty) = VecTy (cntTy ty)
      cntTy ty = error $ "[CgConj] @conjInvWishMvNorm 2 | Shouldn't happen " ++ pprShow ty

      diffTy (VecTy RealTy) = VecTy RealTy
      diffTy (VecTy ty) = VecTy (diffTy ty)
      diffTy ty = error $ "[CgConj] @conjInvWishMvNorm 3 | Shouldn't happen " ++ pprShow ty
conjInvWishMvNorm _ _ =
    error $ "[CgConj] @conjInvWishMvNorm | Shouldn't happen: "

          
-----------------------------------
-- == Sampling code

cgConjSampArgs :: Dist -> Exp TyId -> [Exp TyId] -> SampM [L.Exp TyId]
cgConjSampArgs p_dist p_ept p_es =
    do stats <- asks sr_stats
       idxs <- asks sr_idxs
       let es = map cgExp p_es ++ map (prj idxs) stats 
       case p_dist of
         Dirichlet -> return es
         MvNormal ->
             do l_fn <- asks sr_l_fn
                let l_es = gatherDensArgs (RW.collapseFn l_fn)
                -- TODO: Is patching sound?
                return $ es ++ [ cgExp (patch (l_es !! 1) (densPtIdx' p_ept) ) ]
         InvWishart -> return es
         _ -> error $ "[CgConj] @cgConjSamp | Not supported yet " ++ pprShow p_dist
    where
      prj [] x = L.Var x
      prj idxs x = L.Proj (L.Var x) (map L.Var idxs)
      
      patch :: Exp TyId -> [Exp TyId] -> Exp TyId
      patch e idxs =
          case e of
            Var x -> Var x
            Proj e' _ -> Proj e' idxs
            _ -> error "[CgConj] @patch | Shouldn't happen"

                   
conjSampBody :: Fn TyId -> SampM (L.Stmt TyId)
conjSampBody (Dens p_dist p_ept p_es) =
    do l_dist <- asks sr_l_dist       
       es' <- cgConjSampArgs p_dist p_ept p_es
       let e_rhs = L.DistOp (Conj l_dist Nothing) DM_Fn p_dist es'
           s = L.Store (densPtVar p_ept) (map cgExp (densPtIdx' p_ept)) L.Update e_rhs
       return s
conjSampBody (Ind fn conds) =
    do let conds' = map cgIndCond conds
       s <- conjSampBody fn
       return $ L.If (andExp conds') s L.Skip 
conjSampBody (Let x e fn) =                
    do let s1 = L.Assign x (cgExp e)
       s2 <- conjSampBody fn
       return $ L.seqStmt [ s1, s2 ]       
conjSampBody (Prod fn1 fn2) =
    do s1 <- conjSampBody fn1
       s2 <- conjSampBody fn2
       return $ L.seqStmt [ s1, s2 ]
conjSampBody (Pi x gen fn) =
    do s <- withIdx x (conjSampBody fn)
       return $ L.Loop L.Parallel x (cgGen gen) s


conjSamp :: TyId -> Fn TyId -> SampM (L.Decl TyId)
conjSamp v_mod fn =
    do body <- conjSampBody fn
       stats <- asks sr_stats
       return $ L.Fun (mkCompName "conj_samp" (varName v_mod)) [] stats body Nothing UnitTy
              

              
-----------------------------------
-- == Top-level

selectConj :: Fn TyId -> Fn TyId -> Either String (DensInfo -> DensInfo -> ConjM (L.Stmt TyId), Dist)
selectConj = go
    where
      go fn@(Dens dist pt _) fn'@(Dens dist' _ es') =
          case (dist, dist') of
            (Dirichlet, Categorical) ->
                if pt =\= head es'
                then Right (conjDirCat, dist')
                else Left $ "No conjugacy relation found between "
                         ++ pprShow fn ++ " and " ++ pprShow fn'
            (MvNormal, MvNormal) ->
                if pt =\= head es'
                then Right (conjMvNormMvNorm, dist')
                else Left $ "No conjugacy relation found between "
                         ++ pprShow fn ++ " and " ++ pprShow fn'
            (InvWishart, MvNormal) ->
                if pt =\= (es' !! 1)
                then Right (conjInvWishMvNorm, dist')
                else Left $ "No conjugacy relation found between "
                         ++ pprShow fn ++ " and " ++ pprShow fn'
            _ -> Left $ "No conjugacy relation found between "
                     ++ pprShow fn ++ " and " ++ pprShow fn'
      go fn@Dens{} (Ind fn' [ CatCond x' e' ]) =
          -- go fn (substAExpFn (Var x') e' fn')
          go (subst x' e' fn) fn'
      go fn@Dens{} (Pi _ _ fn') =
          go fn fn'
      go (Let x e fn) fn' =
          go (subst x e fn) fn'
      go fn (Let x' e' fn') =
          go fn (subst x' e' fn')
      go (Pi x gen fn) (Pi x' gen' fn')
          | x == x' && gen =\= gen' = go fn fn'
          | otherwise = Left $ "[CgConj] | Product, could not match " ++ pprShow (Pi x gen fn) ++ " with " ++ pprShow (Pi x' gen' fn')
      go fn fn' =
          Left $ "[CgConj] | Default, could not match " ++ pprShow fn ++ " with " ++ pprShow fn'

               
chkConjM :: (Monad m) => Fn TyId -> Fn TyId -> m Bool
chkConjM fn@(Dens dist pt _) fn'@(Dens dist' _ es') =
    case (dist, dist') of
      (Dirichlet, Categorical) -> chk          
      (MvNormal, MvNormal) -> chk
      (InvWishart, MvNormal) -> chk   
      _ -> do traceM $ "No conjugacy relation found between "
                         ++ pprShow fn ++ " and " ++ pprShow fn'
              return False
    where
      chk = if pt =\= head es'
            then do traceM $ "Succeed " ++ pprShow fn ++ " and " ++ pprShow fn'
                    return True
            else do traceM $ "No conjugacy relation found between "
                               ++ pprShow fn ++ " and " ++ pprShow fn'
                    return False            
chkConjM fn@Dens{} (Ind fn' [ CatCond x' e' ]) =
    chkConjM (subst x' e' fn) fn'
chkConjM fn@Dens{} (Pi x gen fn') =
    do traceM $ "CHKCONJ " ++ pprShow fn ++ " and " ++ pprShow (Pi x gen fn')
       chkConjM fn fn'
chkConjM (Let x e fn) fn' =
    do traceM $ "CHKCONJ " ++ pprShow (subst x e fn) ++ " and " ++ pprShow fn'
       chkConjM (subst x e fn) fn'
chkConjM fn (Let x' e' fn') =
    do traceM $ "CHKCONJ " ++ pprShow fn ++ " and " ++ pprShow (subst x' e' fn')
       chkConjM fn (subst x' e' fn')
chkConjM (Pi x gen fn) (Pi x' gen' fn')
    | x == x' && gen =\= gen' = chkConjM fn fn'
    | otherwise = do traceM $ "Could not match " ++ pprShow fn
                                ++ " with " ++ pprShow fn'
                     return False
chkConjM fn fn' =
    do traceM $ "Could not match " ++ pprShow fn ++ " with " ++ pprShow fn'
       return False

              
chkConj :: Fn TyId -> Fn TyId -> Bool
chkConj p_fn l_fn = runIdentity (chkConjM p_fn l_fn)

              
conjFn :: CompInfo -> CompOpt -> InferCtx TyId -> TyId -> Fn TyId -> CompM (K.Kern (LX.LowPP TyId) TyId)
conjFn cinfo copt inferCtx v_mod fn =
    do let (p_fn, l_fns) = RW.split v_mod fn
           l_fn = prodFn l_fns
       debugM "[Core.CgConj]" $ "@conjFn | Prior for " ++ pprShow v_mod ++ " is " ++ pprShow p_fn
       debugM "[Core.CgConj]" $ "@conjFn | Likelihood for " ++ pprShow v_mod ++ " is " ++ pprShow l_fn
       _ <- chkConjM p_fn l_fn
       case selectConj p_fn l_fn of
         Left errMsg -> throwError errMsg
         Right (cg, l_dist) ->
             do let conjRdr = CR (getGenSym cinfo)
                    conjSt = CS Set.empty []                    
                ((shpM, stat), _, _) <- runRWST (conjStat v_mod cg p_fn l_fn) conjRdr conjSt
                let sampRdr = SR l_dist [] l_fn (L.declAllocs stat)
                (samp, _, _) <- runRWST (conjSamp v_mod p_fn) sampRdr ()
                stat' <- runLint copt stat (Lint.runLintDecl cinfo False inferCtx)
                samp' <- runLint copt samp (Lint.runLintDecl cinfo False inferCtx)
                let like' = samp' -- TODO: Hack, but never used
                    stat'' = LX.LowPP (LX.LowXX shpM False (LX.HostCall False) [] stat')
                    samp'' = LX.LowPP (LX.LowXX shpM False (LX.HostCall False) [] samp')
                    kind = K.Gibbs (K.Conj stat'' samp'')
                    allocs = map (\v -> (v, K.Reset)) (L.declAllocs stat)
                    like'' = error $ "[Core.CgConj] | Shouldn't dereference"-- LX.LowPP (LX.LowXX shpM False LX.HostCall like') -- TODO: Hack, but never used
                    kern = K.Base kind (K.Single v_mod) fn allocs [] like''
                return kern

                       
runConjFn :: CompInfo -> CompOpt -> InferCtx TyId-> TyId -> Fn TyId -> CompM (K.Kern (LX.LowPP TyId) TyId)
runConjFn = conjFn
