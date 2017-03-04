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

module Low.RnLow where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.RWS
import Data.Traversable (traverse)

import AstUtil.Fresh
import AstUtil.Var
import Compile.CompData
import Core.CoreSyn
import Low.LowSyn
import Core.CoreTySyn
import qualified Low.LintLow as Lint
    
----------------------------------------------------------------------
-- = RnLow Description
{-| [Note]

Rename the variables in a Low declaration, i.e., alpha convert.

-}



-----------------------------------
-- == Types and operations

type RnM b = RWST (RnRdr b) [()] (Map.Map String b) CompM
data RnRdr b = RR { rr_varsM :: Map.Map String b
                  , rr_genSym :: GenSym }

withVars :: (Show b, BasicVar b) => Map.Map String b -> RnM b a -> RnM b a
withVars varsM = local (\rdr -> rdr { rr_varsM = varsM })

freshId :: (Show b, BasicVar b) => String -> IdKind -> RnM b b
freshId x ik =
    do genSym <- asks rr_genSym
       lift $ lift $ mkIdIO genSym (mkName x) ik


            
-----------------------------------
-- == Transformation
            
rnId :: (Show b, BasicVar b) => String -> RnM b b
rnId x =
    do varsM <- asks rr_varsM
       case Map.lookup x varsM of
         Just x' -> return x'
         Nothing -> throwError $ "@convId | lookup of " ++ show x ++ " failed in ctx: " ++ show varsM

                    
rnExp :: (Show b, BasicVar b) => Exp String -> RnM b (Exp b)
rnExp e = traverse rnId e

          
rnGen :: (Show b, BasicVar b) => Gen String -> RnM b (Gen b)
rnGen gen = traverse rnId gen

            
rnStmt :: (Show b, BasicVar b) => Stmt String -> RnM b (Stmt b)
rnStmt Skip = return Skip
rnStmt (Exp e) =
    do e' <- rnExp e
       return $ Exp e'
rnStmt (Assign x e) =
    do varsM <- get
       e' <- withVars varsM (rnExp e)
       x' <- freshId x Local
       put (Map.insert x x' varsM)
       return $ Assign x' e'
rnStmt (Store x es uk e) =
    do varsM <- get
       x' <- withVars varsM (rnId x)
       e' <- withVars varsM (rnExp e)
       es' <- mapM (\e'' -> withVars varsM (rnExp e'')) es
       return $ Store x' es' uk e'
rnStmt (Seq s1 s2) =
    do s1' <- rnStmt s1
       s2' <- rnStmt s2
       return $ Seq s1' s2'
rnStmt (If e s1 s2) =
    do e' <- rnExp e
       s1' <- rnStmt s1
       s2' <- rnStmt s2
       return $ If e' s1' s2'
rnStmt (Loop lk x gen s) =
    do varsM <- get
       x' <- freshId x Local
       gen' <- withVars varsM (rnGen gen)
       put (Map.insert x x' varsM)
       s' <- rnStmt s
       put varsM
       return $ Loop lk x' gen' s'
rnStmt (MapRed acc x gen s e) =
    do varsM <- get
       acc' <- freshId acc Local
       x' <- freshId x Local
       gen' <- withVars varsM (rnGen gen)
       let varsM' = Map.insert x x' (Map.insert acc acc' varsM)
       put varsM'
       s' <- rnStmt s
       e' <- withVars varsM' (rnExp e)
       put varsM
       return $ MapRed acc' x' gen' s' e'

              
rnDecl :: (Show b, BasicVar b) => Decl String -> RnM b (Decl b)
rnDecl (Fun name params allocs body retExp retTy) =
    do varsM <- get
       paramsM <- mapM (\(x, _) -> do x' <- freshId x Local
                                      return (x, x')
                       ) params
       allocsM <- mapM (\x -> do x' <- freshId x Local
                                 return (x, x')
                       ) allocs
       let params' = zip (map snd paramsM) (map snd params)
           allocs' = map snd allocsM
           varsM' = varsM `Map.union` Map.fromList paramsM `Map.union` Map.fromList allocsM
       put varsM'
       body' <- rnStmt body
       varsM'' <- get
       retExp' <- case retExp of
                    Just e -> withVars varsM'' (rnExp e) >>= (return . Just)
                    Nothing -> return Nothing
       return $ Fun name params' allocs' body' retExp' retTy


              
-----------------------------------
-- == Top-level


runRnDeclTyVar' :: CompInfo -> CompOpt -> Map.Map String (TVar Typ) -> ModDecls (TVar Typ) -> Decl String -> CompM (Decl (TVar Typ))
runRnDeclTyVar' cinfo copt varsM modDecls decl =
    do let genSym = getGenSym cinfo       
           -- varsM = Map.fromList (map (\(x, _, x', _) -> (x, x')) modDecls)
       (v, _, _) <- runRWST (rnDecl decl) (RR varsM genSym) varsM
       runLint copt v (Lint.runLintDecl cinfo False (IC Map.empty Map.empty modDecls))

runRnDeclTyVar :: CompInfo -> CompOpt -> Map.Map String (TVar Typ) -> ModDecls (TVar Typ) -> Decl String -> IO (Either String (Decl (TVar Typ)))
runRnDeclTyVar cinfo copt varsM modDecls decl =
    runExceptT (runRnDeclTyVar' cinfo copt varsM modDecls decl)
    
