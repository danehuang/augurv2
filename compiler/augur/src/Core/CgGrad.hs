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

{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

module Core.CgGrad(runGradFn) where

import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Pretty
import AstUtil.Var
import Core.DensSyn
import Comm.DistSyn
import Comm.Prim
import qualified Low.LowSyn as L
import Low.LowpPrimSyn
import Core.CoreSyn
import Core.CgCore
import Core.CoreTySyn
import Core.DepCore
import qualified Low.LintLow as Lint
import Compile.CompData
import qualified Core.Graph as G
import qualified Low.LowShpSyn as S
    
       
----------------------------------------------------------------------
-- = CgGrad Description
{-| [Note]

Generate gradient of log-likelihood function from density function.

Dens (TVar Typ)        (simpl expr, types)
       |
Low P.Prim (TVar Typ)  (simpl expr, types)

-}



-----------------------------------
-- == Types and operations

type GradM = RWST GradRdr [()] GradSt CompM
type TyId = L.TyId
data GradRdr =
    GradRdr { gr_depGraph :: G.Graph TyId
            , gr_vWrt :: [TyId]
            , gr_genSym :: GenSym }

data GradSt =
    GradSt { gs_varsM :: Map.Map TyId TyId
           , gs_adj :: Map.Map TyId TyId 
           }
  
              
extendVarsM :: TyId -> TyId -> GradM ()
extendVarsM v v_adj =
    modify (\st -> st { gs_varsM = Map.insert v v_adj (gs_varsM st) })

           
emitAdj :: TyId -> TyId -> GradM ()
emitAdj v_adj v =
    modify (\st -> st { gs_adj = Map.insert v_adj v (gs_adj st) })


getAdj :: GradM (Map.Map TyId TyId)
getAdj = gets gs_adj
         
restoreAdj :: Map.Map TyId TyId -> GradM ()
restoreAdj adjs = modify (\st -> st { gs_adj = adjs })

           
freshId :: Name -> IdKind -> Typ -> GradM TyId
freshId name ik ty =
    do genSym <- asks gr_genSym
       lift $ lift $ mkTyIdIO genSym name ik ty   


mkAdjId' :: IdKind -> TyId -> GradM TyId
mkAdjId' ik v =
    do v_adj <- freshId Anon ik (fromJust $ getType v)
       extendVarsM v v_adj
       emitAdj v_adj v
       return v_adj


initZero :: Typ -> L.Exp TyId -> L.Exp TyId
initZero ty e_cpy =
    case ty of
      RealTy -> L.Lit (L.Real 0)
      VecTy RealTy -> L.Call (L.PrimId DM_Fn PM_Fn ReadVecFromShape) [ e_cpy ]
      MatTy RealTy -> L.Call (L.PrimId DM_Fn PM_Fn ReadMatFromShape) [ e_cpy ]
      _ -> error $ "[CgGrad] @initZero | Unsupported " ++ pprShow e_cpy ++ " with type: " ++ pprShow ty
              
initAdj :: TyId -> TyId -> L.Exp TyId
initAdj x_adj x = initZero (getType' x_adj) (L.Var x)
    {-
    case getType' x_adj of
      RealTy -> L.Lit (L.Real 0)
      VecTy RealTy -> L.Call (L.PrimId DM_Fn PM_Fn ReadVecFromShape) [ L.Var x ]
      MatTy RealTy -> L.Call (L.PrimId DM_Fn PM_Fn ReadMatFromShape) [ L.Var x ]
      t -> error $ "[CgGrad] @initAdj | Unsupported " ++ pprShow x ++ " with type: " ++ pprShow t
     -}

           
mkAdjId :: TyId -> GradM (L.Stmt TyId, TyId)
mkAdjId v =
    do varM <- gets gs_varsM
       case Map.lookup v varM of
         Just v_adj -> return (L.Skip, v_adj)
         Nothing ->
             do v_adj <- mkAdjId' Local v
                traceM $ "Creating adjoint " ++ pprShow v_adj ++ " because lookup of " ++ pprShow v ++ " failed in context " ++ pprShow varM
                return (L.Assign v_adj (initAdj v_adj v), v_adj)


chkGenAdj :: G.Graph TyId -> [TyId] -> TyId -> Bool
chkGenAdj depG v_wrt v =
    (isModParam (idKind v) || isModLocal (idKind v))
    && (elem v v_wrt || any (G.path depG v) v_wrt)

       
chkGenAdjM :: TyId -> GradM Bool
chkGenAdjM v =
    do depG <- asks gr_depGraph
       v_wrt <- asks gr_vWrt
       return $ chkGenAdj depG v_wrt v


-----------------------------------
-- == Transformation
                  
incAdj :: TyId -> [L.Exp TyId] -> L.Exp TyId -> GradM (L.Stmt TyId)
incAdj v_adj idxs erhs =
    case idKind v_adj of
      Local -> 
          do traceM $ "LOCAL: " ++ show v_adj
             return $ L.Store v_adj idxs L.Inc erhs
      _ -> 
          do traceM $ "OTHER: " ++ show v_adj
             return $ L.Store v_adj idxs L.AtmInc erhs
            



-- TODO: UGHHH Refactor me           
gradDist :: Bool -> TyId -> Dist -> [Exp TyId] -> GradM (L.Stmt TyId)
gradDist top x dist es =
    do let es' = map cgExp es
       stmts <- mapM (\(e, idx) -> f es' (idxToDop idx) e) (zip es [0..])
       return $ L.seqStmt stmts
    where
      idxToDop 0 = DotPt
      idxToDop 1 = DotArg1
      idxToDop 2 = DotArg2
      idxToDop i = error $ "[CgGrad] @ gradDist | Cannot take gradient of argument " ++ show i
      
      f es' dop e =
          case e of
            Var y -> 
                do traceM $ "VAR GRAD " ++ pprShow y
                   b <- chkGenAdjM y
                   traceM $ "chkGenAdj: " ++ pprShow (L.Assign x (L.DistOp LL DM_Fn dist es')) ++ " is " ++ show b ++ " for argument " ++ pprShow y
                   if b
                   then
                       if top
                       then
                           do (s_adj, adj_y) <- mkAdjId y
                              let e_rhs = L.DistOp dop DM_Fn dist es'
                              s <- incAdj adj_y [] e_rhs
                              return $ L.seqStmt [ s_adj, s ]
                       else
                           do (s_adj_x, adj_x) <- mkAdjId x
                              (s_adj_y, adj_y) <- mkAdjId y
                              let e_rhs = L.Var adj_x * L.DistOp dop DM_Fn dist es'
                              s <- incAdj adj_y [] e_rhs
                              return $ L.seqStmt [ s_adj_x, s_adj_y, s ]
                   else return L.Skip
            Proj (Var y) idxs ->
                do traceM $ "PROJ GRAD " ++ pprShow y
                   b <- chkGenAdjM y
                   traceM $ "chkGenAdj: " ++ pprShow (L.Assign x (L.DistOp LL DM_Fn dist es')) ++ " is " ++ show b ++ " for argument " ++ pprShow y
                   if b && top
                   then
                        do v <- freshId Anon Local (projBaseTy (getType' y) idxs)
                           let s_v1 = L.Assign v (initZero (getType' v) (cgExp (mkDensPt y idxs)))
                           (s_adj, adj_y) <- mkAdjId y
                           let e_rhs = L.DistOp dop DM_Fn dist es'
                               -- s_v2 = L.Store v [] L.AtmInc e_rhs
                           s_v2 <- incAdj v [] e_rhs
                           s <- incAdj adj_y (map cgExp idxs) (L.Var v)
                           return $ L.seqStmt [ s_v1, s_adj, s_v2, s ]
                   else return L.Skip
            _ -> return L.Skip


gradDotProd :: TyId -> [Exp TyId] -> GradM (L.Stmt TyId)
gradDotProd x es =
    do let es' = map cgExp es             
       stmts <- mapM (\(e, idx) -> f es' idx e) (zip es [0..])
       return $ L.seqStmt stmts
    where
      f es' pos e =
          case e of
            Var y ->
                do b <- chkGenAdjM y
                   if b
                   then
                       do (s_adj_x, adj_x) <- mkAdjId x
                          (s_adj_y, adj_y) <- mkAdjId y
                          let e_rhs = L.Call (L.PrimId DM_Fn (PM_Grad pos) DotProd) (L.Var adj_x : es')
                          s <- incAdj adj_y [] e_rhs
                          return $ L.seqStmt [ s_adj_x, s_adj_y, s ]
                   else return L.Skip
            _ -> return L.Skip

            
gradPrim :: TyId -> [Exp TyId] -> Prim -> GradM (L.Stmt TyId)
gradPrim x es prim =
    case prim of
      DotProd -> gradDotProd x es
      _ ->
          do let es' = map cgExp es
             stmts <- mapM (\(e, idx) -> f es' idx e) (zip es [0..])
             return $ L.seqStmt stmts
    where
      f es' pos e =
          case e of
            Var y ->
                do b <- chkGenAdjM y
                   if b
                   then
                       do (s_adj_x, adj_x) <- mkAdjId x
                          (s_adj_y, adj_y) <- mkAdjId y
                          let e_rhs = L.Var adj_x * L.Call (L.PrimId DM_Fn (PM_Grad pos) prim) es'
                          s <- incAdj adj_y [] e_rhs
                          return $ L.seqStmt [ s_adj_x, s_adj_y, s ]
                   else return L.Skip
            _ -> return L.Skip

                                          
gradAtmExp :: TyId -> Exp TyId -> GradM (L.Stmt TyId)
gradAtmExp x e =
    case e of
      Var y ->
          do b <- chkGenAdjM y
             if b
             then
                 do (s_adj_x, adj_x) <- mkAdjId x
                    (s_adj_y, adj_y) <- mkAdjId y
                    s <- incAdj adj_y [] (L.Var adj_x)
                    return $ L.seqStmt [ s_adj_x, s_adj_y, s ]
             else return L.Skip
      Lit _ ->
          return L.Skip
      DistOp LL dist es ->
          case dist of
            Dirac -> error $ "[CgGrad] | Shouldn't happen: " ++ pprShow (DistOp LL dist es)
            _ -> gradDist False x dist es
      Call (FnId _) _ ->
          error $ "[CgGrad] @gradAtmExp | Unsupported: " ++ pprShow e
      Call (PrimId prim) es ->
          gradPrim x es prim
      Proj (Var y) es ->
          do -- chkAdjExistsOpt
             b <- chkGenAdjM y
             if b
             then
                 do (s_adj_x, adj_x) <- mkAdjId x
                    (s_adj_y, adj_y) <- mkAdjId y
                    s <- incAdj adj_y (map cgExp es) (L.Var adj_x)
                    return $ L.seqStmt [ s_adj_x, s_adj_y, s ]
             else return L.Skip
          where
            chkAdjExistsOpt =
                do varsM <- gets gs_varsM
                   case Map.lookup y varsM of
                     Just v_adj -> extendVarsM x v_adj
                     Nothing -> return ()
      e' -> error $ "Should not happen: " ++ pprShow e'
         
                       
gradInd :: IndCond TyId -> GradM (L.Exp TyId)
gradInd (CatCond x e) =
    return $ L.Call (L.PrimId DM_Fn PM_Fn EqEq) [ L.Var x, cgExp e ]

                                  
gradFnBody :: Fn TyId -> GradM (L.Stmt TyId)
gradFnBody (Dens Dirac _ es) =
    do let e = head es
           y = case e of
                 Var y' -> y'
                 _ -> error $ "[CgGrad] @gradFnBody| Shouldn't happen"
       b <- chkGenAdjM y
       if b
       then
           do (s_adj_y, adj_y) <- mkAdjId y
              s <- incAdj adj_y [] (cgExp e)
              return $ L.seqStmt [ s_adj_y, s ]
       else return L.Skip          
gradFnBody (Dens dist pt es) =
    gradDist True (densPtVar pt) dist (pt : es)
gradFnBody (Ind fn conds) =
    do conds' <- mapM gradInd conds
       s <- gradFnBody fn
       return $ L.If (andExp conds') s L.Skip   
gradFnBody (Let x e fn) =
    do let s1 = L.Assign x (cgExp e)
       s2 <- gradFnBody fn
       s3 <- gradAtmExp x e
       return $ L.seqStmt [ s1, s2, s3 ]
gradFnBody (Prod fn1 fn2) =
    do saved <- getAdj
       s1 <- gradFnBody fn1
       restoreAdj Map.empty
       s2 <- gradFnBody fn2
       restoreAdj saved
       return $ L.seqStmt [ s1, s2 ]
gradFnBody (Pi x gen fn) =
    do saved <- getAdj
       restoreAdj Map.empty
       s <- gradFnBody fn
       restoreAdj saved
       return $ L.Loop L.AtomicPar x (cgGen gen) s


gradFnDecl :: Name -> Fn TyId -> GradM (S.ShpCtx TyId, [TyId], L.Decl TyId)
gradFnDecl name fn =
    do v_wrt <- asks gr_vWrt
       vs_adj <- mapM (mkAdjId' ModAux) v_wrt
       restoreAdj Map.empty
       body <- gradFnBody fn
       let allocs = map (\(adj_x, x) -> setType adj_x (getType' x)) (zip vs_adj v_wrt)
           shpM = Map.fromList (map (\(adj_x, x) ->
                                         case getType' x of
                                           IntTy -> (adj_x, S.Scalar)
                                           RealTy -> (adj_x, S.Scalar)
                                           _ -> (adj_x, S.mkCpy x)
                                    ) (zip vs_adj v_wrt))
           name' = mkName (concatMap (nameToStr . varName) v_wrt)
       return (shpM, vs_adj, L.Fun (mkCompName "grad" name') [] allocs body Nothing UnitTy)


-----------------------------------
-- == Top-level


runGradFn :: CompInfo -> CompOpt -> InferCtx TyId -> Name -> [TyId] -> Fn TyId -> CompM (S.ShpCtx TyId, [TyId], L.Decl TyId)
runGradFn cinfo copt inferCtx name v_wrt fn =
    do let depG = depFnG fn
           rdr = GradRdr depG v_wrt (getGenSym cinfo)
           st = GradSt Map.empty Map.empty
       debugM "Core.CgGrad" $ "Dens (Input):\n" ++ pprShow fn
       ((shpCtx, vs_adj, grad), _, _) <- runRWST (gradFnDecl name fn) rdr st
       debugM "Core.CgGrad" $ "LowPP (Intermediate):\n" ++ pprShow grad
       grad' <- runLint copt grad (Lint.runLintDecl cinfo False inferCtx)
       debugM "Core.CgGrad" $ "LowPP (Output):\n" ++ pprShow grad'
       return (shpCtx, vs_adj, grad')


                                   
{-           
initAdjs :: Map.Map TyId TyId -> L.Stmt TyId
initAdjs adjs =
    L.seqStmt (map (\(x_adj, x) -> L.Assign x_adj (f x_adj x)) (Map.toList adjs))
    where
      f x_adj x =
          case getType' x_adj of
            RealTy -> L.Lit (L.Real 0)
            VecTy RealTy -> L.Call (L.PrimId DM_Fn PM_Fn ReadVecFromShape) [ L.Var x ]
            MatTy RealTy -> L.Call (L.PrimId DM_Fn PM_Fn ReadMatFromShape) [ L.Var x ]
            t -> error $ "[CgGrad] @initAdjs | Unsupported " ++ pprShow x ++ " with type: " ++ pprShow t
-}
