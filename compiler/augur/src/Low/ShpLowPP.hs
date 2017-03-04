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

module Low.ShpLowPP(runShpiDecl) where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Var
import Comm.DistSyn
import Comm.Prim
import Compile.CompData
import Core.CoreSyn
import Low.LowSyn
import Low.LowpPrimSyn
import qualified Low.LowShpSyn as Shp
import Core.CoreTySyn

    
----------------------------------------------------------------------
-- = ShpLow description
{-| [Note]


Low M.Prim (TVar Typ)   (linearized exp, types, allocs)
          |
ShpCtx (TVar Typ)       (shape of allocs)

-}


-----------------------------------
-- == Types and operations

type ShpM b = StateT (Shp.ShpCtx b) CompM


-----------------------------------
-- == Inference
    
shpiExp :: (TypedVar b Typ) => Exp b -> ShpM b (Shp.Shp b)
shpiExp (Var x) =
    do shpCtx <- get
       case Map.lookup x shpCtx of
         Just shp -> return shp
         Nothing -> throwError $ "@shpiExp | Lookup of " ++ pprShow x ++ " failed in context " ++ pprShow shpCtx
shpiExp (Lit lit) =
    case lit of
      Int _ -> return Shp.mkScalar
      Real _ -> return Shp.mkScalar
shpiExp (DistOp _ dm dist es) =
    case dm of
      DM_Fn -> throwError $ "@shpiExp | Should not have functional distributions at this point"
      DM_Mem -> 
          case dist of
            Dirichlet -> shpiExp (es !! 0)
            MvNormal -> shpiExp (es !! 0)
            InvWishart -> shpiExp (es !! 0)
            _ -> return Shp.mkScalar
shpiExp (Call ce es) =
    case ce of
      FnId _ -> error $ "TODO: currently unsupported"
      PrimId dm pm prim ->
          case prim of
            AllocVecFromShape ->
                do -- shp <- shpiExp
                   error "Not used? AllocVecFromShape"
            ReadVecFromShape ->
                do shp <- shpiExp (es !! 1)
                   updateGlob shp (es !! 0)
                   return $ shp
            AllocMatFromShape -> error "Not used? AllocMatFromShape"
            ReadMatFromShape ->
                do shp <- shpiExp (es !! 1)
                   updateGlob shp (es !! 0)
                   return $ shp
            _ -> return Shp.mkScalar
    where
      updateGlob shp (Var x) = modify (\shpCtx -> Map.insert x shp shpCtx)
      updateGlob _ e = throwError $ pprShow e
                       
shpiExp (Proj e es) =
    do shp <- shpiExp e
       -- return $ Shp.projShp (Shp.splatShp shp) (length es)
       let shp' = Shp.projShp shp (length es)
       traceM $ "Inferred SHP for " ++ pprShow (Proj e es) ++ " is " ++ pprShow shp' ++ " where original " ++ pprShow e ++ " is " ++ pprShow shp
       return shp'

                        
shpiStmt :: (TypedVar b Typ) => Stmt b -> ShpM b ()
shpiStmt Skip = return ()
shpiStmt (Exp _) = return ()
shpiStmt (Assign x e) =
    do shp <- shpiExp e
       modify (\shpCtx -> Map.insert x shp shpCtx)
shpiStmt (Store _ _ _ _) = return ()
shpiStmt (Seq s1 s2) =
    do shpiStmt s1
       shpiStmt s2
shpiStmt (If e s1 s2) =
    do shpiStmt s1
       shpiStmt s2
shpiStmt (Loop _ x _ s) =
    do modify (\shpCtx -> Map.insert x Shp.mkScalar shpCtx)
       shpiStmt s
shpiStmt (MapRed acc x _ s e) =
    do modify (\shpCtx -> Map.insert x Shp.mkScalar shpCtx)
       shpiStmt s
       shp <- shpiExp e
       modify (\shpCtx -> Map.insert acc shp shpCtx)
              
                                  
shpiDecl :: (TypedVar b Typ) => Decl b -> ShpM b ()
shpiDecl (Fun name params allocs body retExp _) =
    do modify (\ctx -> ctx `Map.union` initShpCtx)
       shpiStmt body
       shp <- case retExp of
                Just e -> shpiExp e
                Nothing -> return Shp.mkScalar
       return ()
    where
      initShpCtx =
          Map.fromList (map (\(x, ty) ->
                                 case getType' x of
                                   IntTy -> (x, Shp.Scalar)
                                   RealTy -> (x, Shp.Scalar)
                                   _ -> (x, Shp.mkCpy x)
                            ) params)

             

-----------------------------------
-- == Top-level
                         
runShpiDecl :: (TypedVar b Typ) => InferCtx b -> Shp.ShpCtx b -> Decl b -> CompM (Shp.ShpCtx b)
runShpiDecl inferCtx shpCtx decl =
    do let modDecls = ic_modDecls inferCtx
           dupCtx = ic_dupCtx inferCtx
           modDeclsShp = Map.fromList (map (\(_, x, _) -> (x, Shp.mkCpy x)) modDecls)
           dupCtxShp = Map.fromList (map (\v -> (fromJust (Map.lookup v dupCtx), Shp.mkCpy v)) (getModParamIds modDecls))
       (_, shpCtx') <- runStateT (shpiDecl decl) (modDeclsShp `Map.union` dupCtxShp `Map.union` shpCtx)
       let allocs = Set.fromList (declAllocs decl)
       return $ Map.filterWithKey (\k _ -> k `Set.member` allocs) shpCtx'
