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

module Low.ProjLow 
    ( runProjStmt
    , runProjDecl ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Pretty
import AstUtil.Var
import Compile.CompData
import Core.CoreSyn
import Low.LowSyn
import Core.CoreTySyn
import qualified Low.LintLow as Lint


----------------------------------------------------------------------
-- = ProjLow Description
{-| [Note]

Unfold projections in a Low declaration.
Has type information.
In the future, put vector/mat type conversions here?
Also put top-level vectors here

-}



-----------------------------------
-- == Types and operations

type ProjM' b = WriterT [Assign b] ProjM
type ProjM = ReaderT GenSym CompM


freshId :: (TypedVar b t) => ProjM b
freshId =
    do genSym <- ask
       liftIO $ mkIdIO genSym Anon Local

    
freshId' :: (TypedVar b t) => ProjM' b b
freshId' =
    do genSym <- ask
       lift $ liftIO $ mkIdIO genSym Anon Local


bindExp :: (TypedVar b Typ) => Exp b -> ProjM' b (Exp b)
bindExp e =
    do v <- freshId'
       tell [(v, e)]
       return (Var v)

        
-----------------------------------
-- == Transformations

projExp :: (TypedVar b Typ) => Exp b -> ProjM' b (Exp b)
projExp (Var x) =
    return $ Var x
projExp (Lit lit) =
    return $ Lit lit
projExp (Call ce es) =
    do es' <- mapM projExp es
       return $ Call ce es'
projExp (DistOp dop dm dist es) =
    do es' <- mapM projExp es
       return $ DistOp dop dm dist es'
projExp (Proj e es) =
    do e' <- projExp e
       es' <- mapM projExp es
       unfoldProj e' es'
    where
      unfoldProj acc (e':[]) =
          return (Proj acc [e'])
      unfoldProj acc (e':es') =
          do e_proj <- bindExp (Proj acc [e'])
             unfoldProj e_proj es'
             -- acc' <- unfoldProj acc es'
             -- bindExp (Proj acc' [e'])
      unfoldProj _ [] = error $ "@unfoldProj | Projection has empty list"

runProjExp :: (TypedVar b Typ) => Exp b -> ProjM ([Assign b], Exp b)
runProjExp e =
    do (e', assigns) <- runWriterT (projExp e)
       return (assigns, e')

              
projGen :: (TypedVar b Typ) => Gen b -> ProjM ([Assign b], Gen b)
projGen (Until e1 e2) =
    do (assigns1, e1') <- runProjExp e1
       (assigns2, e2') <- runProjExp e2
       return (assigns1 ++ assigns2, Until e1' e2')

              
projStmt :: (TypedVar b Typ) => Stmt b -> ProjM (Stmt b)
projStmt Skip =
    return Skip
projStmt (Exp e) =
    do (assigns, e') <- runProjExp e
       return $ assignsToStmt' assigns (Exp e')
projStmt (Assign x e) =
    do (assigns, e') <- runProjExp e
       return $ assignsToStmt' assigns (Assign x e')
projStmt (Store x es uk e) =
    do (assigns, e') <- runProjExp e
       v <- storeProj
       case v of
         Left (assignsp, Var y) ->
             return $ assignsToStmt' (assigns ++ assignsp) (Store y [] uk e')
         Left (assignsp, Proj (Var y) es') ->
             do y' <- freshId
                let s = (y', (Proj (Var y) es'))                
                return $ assignsToStmt' (assigns ++ assignsp ++ [ s ]) (Store y' [] uk e')
         Right (assignsp, Var y, e_idx') ->
             return $ assignsToStmt' (assigns ++ assignsp) (Store y [e_idx'] uk e')
         Right (assignsp, Proj (Var y) es', e_idx') ->
              do y' <- freshId
                 let s = (y', (Proj (Var y) es'))                 
                 return $ assignsToStmt' (assigns ++ assignsp ++ [ s ]) (Store y' [ e_idx' ] uk e')
         _ -> error $ "[ProjLow] @projStmt | Shouldn't happen: " ++ pprShow (Store x es uk e)
    where
      storeProj =
          case es of
            [] -> return $ Left ([], Var x)
            e_idx : [] ->
                case projBaseTy (getType' x) es of
                  IntTy ->
                      do (assigns, e_idx') <- runProjExp e_idx
                         return $ Right (assigns, Var x, e_idx')
                  RealTy ->
                      do (assigns, e_idx') <- runProjExp e_idx
                         return $ Right (assigns, Var x, e_idx')
                  _ -> runProjExp (Proj (Var x) es) >>= return . Left
            _ -> case projBaseTy (getType' x) es of
                   IntTy ->
                       do (assigns, e') <- runProjExp (Proj (Var x) (init es))
                          (assigns2, e2') <- runProjExp (last es)
                          return $ Right (assigns ++ assigns2, e', e2')
                   RealTy ->
                       do (assigns, e') <- runProjExp (Proj (Var x) (init es))
                          (assigns2, e2') <- runProjExp (last es)
                          return $ Right (assigns ++ assigns2, e', e2')
                   _ -> runProjExp (Proj (Var x) es) >>= return . Left
projStmt (Seq s1 s2) =
    do s1' <- projStmt s1
       s2' <- projStmt s2
       return $ Seq s1' s2'
projStmt (If e s1 s2) =
    do (assigns, e') <- runProjExp e
       s1' <- projStmt s1
       s2' <- projStmt s2
       return $ assignsToStmt' assigns (If e' s1' s2')
projStmt (Loop lk x gen s) =
    do (assigns, gen') <- projGen gen
       s' <- projStmt s
       return $ assignsToStmt' assigns (Loop lk x gen' s')
projStmt (MapRed acc x gen s e) =
    do (assignsg, gen') <- projGen gen
       s' <- projStmt s
       (assignse, e') <- runProjExp e
       let s'' = Seq s' (assignsToStmt assignse)
       return $ assignsToStmt' assignsg (MapRed acc x gen' s'' e')

              
projDecl :: (TypedVar b Typ) => Decl b -> ProjM (Decl b)
projDecl (Fun name params alloc body retExp retTy) =
    do body' <- projStmt body
       (assigns, retExp') <- projRet retExp
       let body'' = Seq body' (assignsToStmt assigns)
       return $ Fun name params alloc body'' retExp' retTy
    where
      projRet (Just e) = runProjExp e >>= \(assigns, e') -> return (assigns, Just e')
      projRet Nothing = return ([], Nothing)


-----------------------------------
-- == Top-level

runProjStmt :: (TypedVar b Typ) => CompInfo -> Stmt b -> CompM (Stmt b)
runProjStmt cinfo s =
    do v <- runReaderT (projStmt s) (getGenSym cinfo)
       return v

runProjDecl :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> Decl b -> CompM (Decl b)
runProjDecl cinfo copt inferCtx decl =
    do debugM "Low.ProjLow" $ "LowPP/LowMM (Input):\n" ++ pprShow decl
       v <- runReaderT (projDecl decl) (getGenSym cinfo)
       debugM "Low.ProjLow" $ "LowPP/LowMM (Intermediate):\n" ++ pprShow v
       v' <- runLint copt v (Lint.runLintDecl cinfo False inferCtx)
       debugM "Low.ProjLow" $ "LowPP/LowMM (Output):\n" ++ pprShow v'
       return v'
