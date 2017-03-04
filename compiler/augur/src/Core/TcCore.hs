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

module Core.TcCore
    ( runTcFn
    , runTcKernU ) where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Traversable as T

import AstUtil.Pretty
import AstUtil.Var
import Comm.DistSyn
import Comm.Prim
import Core.CoreSyn
import Core.DensSyn
import qualified Core.KernSyn as K
import Core.CoreTySyn
import Low.LowpPrimSyn   

    
----------------------------------------------------------------------
-- = TcCore Description
{-| [Note]

Contains type checking for Core programs.

-}



-----------------------------------
-- == Types and operations

type TcM b = ExceptT String (ReaderT (Map.Map b Typ) IO)

withVar :: (TypedVar b Typ) => b -> Typ -> TcM b a -> TcM b a
withVar x ty = local (\varsM -> Map.insert x ty varsM)

    
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
unify (t1, t2) = throwError $ "[TcCore] @unify | Failed to unify: " ++ pprShow t1 ++ " with " ++ pprShow t2

                 
unifyOverload :: [Typ] -> Typ -> TcM b Typ
unifyOverload ts t2 = go ts
    where
      go (t1:tl) = catchError (unify (t1, t2)) (\_ -> go tl)
      go [] = throwError $ "[TcCore] @unifyOverload | None of the overloaded types unify."


unifyOverload2 :: [Typ] -> [Typ] -> TcM b Typ
unifyOverload2 ts1 ts2 =
    case ts2 of
      [] -> throwError $ "[TcCore] @unifyOverload2 | None of the overloaded types unify."
      t2:tl2 -> catchError (unifyOverload ts1 t2) (\_ -> unifyOverload2 ts1 tl2)

              

-----------------------------------
-- == Typecheck
                               
projRetTy :: Typ -> [Typ] -> TcM b Typ
projRetTy t ts
    | length ts > 0 =
        case t of
          VecTy t' -> projRetTy t' (tail ts)
          MatTy t' -> projRetTy (VecTy t') (tail ts)
          _ -> throwError $ "[TcCore] @projRetTy | Cannot project: " ++ pprShow t
    | otherwise = return t

                  
tcExp :: (TypedVar b Typ) => Exp b -> TcM b Typ
tcExp (Var x) = 
    do tyCtx <- ask
       case Map.lookup x tyCtx of
         Just ti -> return ti
         Nothing -> throwError $ "[TcCore] @tcExp | Lookup of variable " ++ pprShow x ++ " failed in ctx: " ++ pprShow tyCtx
tcExp (Lit lit) =
    case lit of
      Int _ -> return IntTy
      Real _ -> return RealTy
tcExp (DistOp dop dist es) =
    do ts <- mapM tcExp es
       let ts' = distArgTys dist                 
       when (length ts /= length ts') (throwError $ "[TcCore] | Argument lengths do not match when checking: " ++ pprShow (DistOp dop dist es))
       case dop of
         LL ->
             do mapM_ unify (zip ts (map injCommTy ts'))
                return $ RealTy
         Pdf ->
             do mapM_ unify (zip ts (map injCommTy ts'))
                return $ RealTy
         Sample ->
             do mapM_ unify (zip ts (map injCommTy ts'))
                return $ injCommTy (distSampTy dist)
         Conj _ _ ->
             do -- TODO: Check arguments
                return $ injCommTy (distSampTy dist)
         _ -> error $ "[TcCore] @tcExp | TODO tc dist " ++ pprShow dop
tcExp (Call ce es) =
    case ce of
      FnId _ -> error $ "[TcCore] @tcExp | Currently not supported"
      PrimId prim ->          
          do ts <- mapM tcExp es
             let arrTys = mkOverloadTys ts (getPrimRetTys DM_Fn PM_Fn prim)
             t <- unifyOverload2 (getPrimTy DM_Fn PM_Fn prim) arrTys
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


tcIndCond :: (TypedVar b Typ) => IndCond b -> TcM b ()
tcIndCond (CatCond x e) =
    do t1 <- tcExp (Var x)
       t2 <- tcExp e
       _ <- unify (t1, IntTy)
       _ <- unify (t2, IntTy)
       return ()

              
instTyp' :: (TypedVar b Typ) => b -> TcM b b
instTyp' x =
    do tyCtx <- ask
       case Map.lookup x tyCtx of
         Just t -> return $ setType x t
         Nothing -> throwError $ "[TcCore] @instTyp' | Lookup of " ++ pprShow x ++ " failed in context: " ++ pprShow tyCtx
           
             
tcFn :: (TypedVar b Typ) => Fn b -> TcM b (Fn b)
tcFn (Dens Dirac pt es) =
    do t <- tcExp pt
       ts <- mapM tcExp es
       _ <- unify (t, ts !! 0)
       pt' <- T.traverse instTyp' pt
       es' <- mapM (T.traverse instTyp') es
       return $ Dens Dirac pt' es'
tcFn (Dens dist pt es) =
    do t <- tcExp pt
       ts <- mapM tcExp es
       let t' = injCommTy (distSampTy dist)
           ts' = map injCommTy (distArgTys dist)
       when (length ts /= length ts') (throwError $ "[tcFn] | Argument lengths do not match when checking: " ++ pprShow (Dens dist pt es))
       mapM_ unify (zip ts ts')
       _ <- unify (t, t')
       pt' <- T.traverse instTyp' pt
       es' <- mapM (T.traverse instTyp') es
       return $ Dens dist pt' es'
tcFn (Ind fn conds) =
    do mapM_ tcIndCond conds
       conds' <- mapM (T.traverse instTyp') conds
       fn' <- tcFn fn
       return $ Ind fn' conds'
tcFn (Let x e fn) =
    do t <- tcExp e
       e' <- T.traverse instTyp' e
       x' <- withVar x t (instTyp' x)
       fn' <- withVar x t (tcFn fn)
       return $ Let x' e' fn'
tcFn (Prod fn1 fn2) =
    do fn1' <- tcFn fn1
       fn2' <- tcFn fn2       
       return $ Prod fn1' fn2'
tcFn (Pi x gen fn) =
    do tcGen gen
       gen' <- T.traverse instTyp' gen
       x' <- withVar x IntTy (instTyp' x)
       fn' <- withVar x IntTy (tcFn fn)
       return $ Pi x' gen' fn'
              
              
tcKernU :: (TypedVar b Typ) => K.KernU b -> TcM b (K.KernU b)
tcKernU (K.Base kind ku fn extra kernParams code) =
    do extra' <- mapM (\(v, ak) -> instTyp' v >>= \v' -> return (v', ak)) extra
       fn' <- tcFn fn
       ku' <- T.traverse instTyp' ku
       kernParams' <- mapM instTyp' kernParams -- TODO: This is always empty?
       return $ K.Base kind ku' fn' extra' kernParams' code    
tcKernU (K.Tensor k1 k2) =
    do k1' <- tcKernU k1
       k2' <- tcKernU k2
       return $ K.Tensor k1' k2'

              
-----------------------------------
-- == Top-level

runTcFn :: (TypedVar b Typ) => ModDecls b -> Fn b -> IO (Either String (Fn b))
runTcFn modDecls fn =
    do let modDeclsCtx = Map.fromList (map (\(_, x, ty) -> (x, ty)) modDecls)
       runReaderT (runExceptT (tcFn fn)) modDeclsCtx

                  
runTcKernU :: (TypedVar b Typ) => ModDecls b -> K.KernU b -> IO (Either String (K.KernU b))
runTcKernU modDecls k =
    do let modDeclsCtx = Map.fromList (map (\(_, x, ty) -> (x, ty)) modDecls)
       runReaderT (runExceptT (tcKernU k)) modDeclsCtx
