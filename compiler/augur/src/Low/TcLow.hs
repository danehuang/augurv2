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

module Low.TcLow where


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Traversable as T
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Var
import Comm.DistSyn
import Comm.Prim
import Core.CoreSyn
import Low.LowSyn
import Core.CoreTySyn
import Low.LowpPrimSyn as P


----------------------------------------------------------------------
-- = TcLow Description
{-| [Note]

Contains type inference for Low programs.

-}



-----------------------------------
-- == Types and operations

type TcM b = ExceptT String (StateT (Map.Map b Typ) IO)


-----------------------------------
-- == Unification
    
unify :: (Typ, Typ) -> TcM b Typ
unify (UnitTy, UnitTy) = return UnitTy
unify (IntTy, IntTy) = return IntTy
unify (RealTy, RealTy) = return RealTy
unify (VecTy t1, VecTy t2) =
    do t <- unify (t1, t2)
       return $ VecTy t
unify (MatTy t1, MatTy t2) =
    do t <- unify (t1, t2)
       return $ MatTy t
unify (ArrTy ts1 t1, ArrTy ts2 t2) =
    do ts <- mapM unify (zip ts1 ts2)
       t <- unify (t1, t2)
       return $ ArrTy ts t
unify (t1, t2) = throwError $ "[TcLow] @unify | Failed to unify " ++ pprShow t1 ++ " with " ++ pprShow t2

                 
unifyOverload :: [Typ] -> Typ -> TcM b Typ
unifyOverload ts t2 = go ts
    where
      go (t1:tl) = catchError (unify (t1, t2)) (\_ -> go tl)
      go [] = throwError $ "[TcLow] @unifyOverload | None of the overloaded types unify: " ++ rendSepBy commasp ts

              
unifyOverload2 :: [Typ] -> [Typ] -> TcM b Typ
unifyOverload2 ts1 ts2 =
    case ts2 of
      [] -> throwError $ "[TcLow] @unifyOverload2 | None of the overloaded types unify."
      t2:tl2 -> catchError (unifyOverload ts1 t2) (\_ -> unifyOverload2 ts1 tl2)


              
-----------------------------------
-- == Typecheck
                               
projRetTy :: Typ -> [Typ] -> TcM b Typ
projRetTy t ts
    | length ts > 0 =
        case t of
          VecTy t' -> projRetTy t' (tail ts)
          MatTy t' -> projRetTy (VecTy t') (tail ts)
          _ -> throwError $ "[TcLow] @projRetTy | Cannot project: " ++ pprShow t
    | otherwise = return t

                  
tcExp :: (TypedVar b Typ) => Exp b -> TcM b Typ
tcExp (Var x) = 
    do tyCtx <- get
       case Map.lookup x tyCtx of
         Just ti -> return ti
         Nothing -> throwError $ "[TcLow] @tcExp | Lookup of variable " ++ pprShow x ++ " failed in ctx: " ++ pprShow tyCtx
tcExp (Lit lit) =
    case lit of
      Int _ -> return IntTy
      Real _ -> return RealTy
tcExp (DistOp dop dm Dirac es) =
    do ts <- mapM tcExp es
       return $ ts !! 0
tcExp (DistOp dop dm dist es) =
    do ts <- mapM tcExp es
       let ts' = map injCommTy (distArgTys' dop dm dist)       
       case dop of
         Conj _ _ ->
             -- TODO: HACK??
             return $ injCommTy (distRetTy' dop dm dist)
         _ ->
             do when (length ts /= length ts') (throwError $ "[TcLow] @tcExp | Could not match argument types " ++ rendSepBy commasp ts ++ " with expected types " ++ rendSepBy commasp ts' ++ " when checking " ++ pprShow (DistOp dop dm dist es))
                mapM_ unify (zip ts ts')             
                return $ injCommTy (distRetTy' dop dm dist)
tcExp (Call ce es) =
    case ce of
      FnId fn -> error $ "[TcLow] @tcExp | FnId currently not supported: " ++ pprShow fn
      PrimId dm pm prim ->
          do ts <- mapM tcExp es                                             
             let -- t = ArrTy ts (getRetTy pm prim)
                 arrTys = mkOverloadTys ts (getPrimRetTys dm pm prim)
             traceM $ "[TcLow] | Checking " ++ pprShow (Call ce es) ++ " with expected type " ++ pprShowLs (getPrimTy dm pm prim) ++ " with actual type " ++ pprShowLs arrTys
             t <- unifyOverload2 (getPrimTy dm pm prim) arrTys
             return $ arrRetTy t          
tcExp (Proj e es) =
    do t <- tcExp e
       ts <- mapM tcExp es
       projRetTy t ts

                 
tcGen :: (TypedVar b Typ) => Gen b -> TcM b ()
tcGen (Until e1 e2) =
    do t1 <- tcExp e1
       t2 <- tcExp e2       
       _ <- unify (t1, IntTy)
       _ <- unify (t2, IntTy)
       return ()

              
tcStmt :: (TypedVar b Typ) => Stmt b -> TcM b ()
tcStmt Skip = return ()
tcStmt (Exp e) =
    do traceM $ "[TcLow] @tcStmt | Checking: " ++ pprShow (Exp e)
       _ <- tcExp e -- Enforce unit?
       return ()
       traceM $ "[TcLow] @tcStmt | Done Checking: " ++ pprShow (Exp e)
tcStmt (Assign x e) =
    do traceM $ "[TcLow] @tcStmt | Checking: " ++ pprShow (Assign x e)
       t <- tcExp e
       modify (\tyCtx -> Map.insert x t tyCtx)
       traceM $ "[TcLow] @tcStmt | Done Checking: " ++ pprShow (Assign x e)
tcStmt (Store x es uk e) =
    do traceM $ "[TcLow] @tcStmt | Checking: " ++ pprShow (Store x es uk e)
       ts <- mapM tcExp es
       mapM_ (\t -> unify (t, IntTy)) ts
       t <- tcExp e
       tyCtx <- get
       case Map.lookup x tyCtx of
         Just t' ->
             do baseTy <- projRetTy t' ts
                _ <- unify (baseTy, t)
                return ()
         Nothing -> throwError $ "[TcLow] @tcStmt | Lookup of " ++ pprShow x ++ " failed in " ++ pprShow tyCtx
tcStmt (Seq s1 s2) =
    do tcStmt s1
       tcStmt s2
tcStmt (If e s1 s2) =
    do traceM $ "[TcLow] @tcStmt | Checking IF: " ++ pprShow (If e s1 s2)
       t <- tcExp e
       _ <- unify (t, IntTy)
       tcStmt s1
       tcStmt s2
       traceM $ "[TcLow] @tcStmt | Done Checking IF: " ++ pprShow (If e s1 s2)
tcStmt (Loop lk x gen s) =
    do traceM $ "[TcLow] @tcStmt | Checking LOOP: " ++ pprShow (Loop lk x gen s)
       tcGen gen
       traceM $ "[TcLow] @tcStmt | Done Checking LOOP gen: " ++ pprShow gen
       modify (\tyCtx' -> Map.insert x IntTy tyCtx')
       tcStmt s
       traceM $ "[TcLow] @tcStmt | Done Checking LOOP: " ++ pprShow (Loop lk x gen s)
tcStmt (MapRed acc x gen s e) =
    do tcGen gen
       modify (\tyCtx' -> Map.insert x IntTy tyCtx')
       tcStmt s
       t <- tcExp e
       modify (\tyCtx' -> Map.insert acc t tyCtx')

              
tcDecl :: (TypedVar b Typ) => Decl b -> TcM b ()
tcDecl (Fun _ params allocs body retExp retTy) =
    do -- mapM_ (\(x, t) -> unify (fromJust $ getType x, t)) params
       traceM $ "TC | ALLOC " ++ pprShowLs allocs
       mapM_ (\x -> traceM $ pprShow x ++ " :: " ++ pprShow (getType' x)) allocs
       let -- Check parameter types match?
           paramTyM = Map.fromList params
           allocTyM = Map.fromList (map (\x -> (x, getType' x)) allocs)
       -- put (Map.union paramTyM allocTyM)
       modify (\ctx -> ctx `Map.union` paramTyM `Map.union` allocTyM)
       tcStmt body
       retTy' <- case retExp of
                   Just e -> tcExp e
                   Nothing -> return UnitTy
       _ <- unify (retTy, retTy')
       return ()


-----------------------------------
-- == Instantiate
              
instTyp :: (TypedVar b Typ) => Map.Map b Typ -> b -> ExceptT String IO b
instTyp tyCtx x =
    case Map.lookup x tyCtx of
      Just t -> return $ setType x t
      Nothing -> throwError $ "[TcLow] @instTyp | Lookup of " ++ pprShow x ++ " failed in context: " ++ pprShow tyCtx

                 
instDecl :: (TypedVar b Typ) => Map.Map b Typ -> Decl b -> ExceptT String IO (Decl b)
instDecl tyCtx decl = T.traverse (instTyp tyCtx) decl


-----------------------------------
-- == Top-level

runTcDecl :: (TypedVar b Typ) => InferCtx b -> Decl b -> IO (Either String (Decl b))
runTcDecl inferCtx decl =
    do traceM $ "[TC] | BEGIN\n"
       let modDecls = ic_modDecls inferCtx
           dupCtx = ic_dupCtx inferCtx
           modDeclsCtx = Map.fromList (map (\(_, x, ty) -> (x, ty)) modDecls)
           dupCtxCtx = Map.fromList (map (\v -> (fromJust (Map.lookup v dupCtx), getType' v)) (getModParamIds modDecls))
       traceM $ "[TC] | ModDecls: " ++ pprShow modDeclsCtx
       traceM $ "[TC] | DupCtx: " ++ pprShow dupCtx
       traceM $ "[TC] | DECL: \n" ++ pprShow decl
       (v, tyCtx) <- runStateT (runExceptT (tcDecl decl)) (modDeclsCtx `Map.union` dupCtxCtx)
       traceM $ "END OF TC" ++ pprShow tyCtx
       case v of
         Left errMsg -> return $ Left errMsg
         Right _ -> runExceptT (instDecl tyCtx decl)
