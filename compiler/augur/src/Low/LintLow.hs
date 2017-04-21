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

module Low.LintLow where

import qualified Data.Map as Map
import Control.Monad.Except
import Debug.Trace

    
import AstUtil.Fresh
import AstUtil.Pretty
import AstUtil.Var
import Compile.CompData
import Core.CoreSyn
import Low.LowSyn
import Core.CoreTySyn
import qualified Low.LinLow as Lin
import qualified Low.TcLow as Tc


----------------------------------------------------------------------    
-- = LintLow description
{-| [Note]

"Lint" a Low declaration, i.e., normalize and check.

-}


-----------------------------------
-- == Types and operations

type LintM = CompM


    
-----------------------------------
-- == Transformations

lintStmt :: (TypedVar b Typ) => GenSym -> Bool -> InferCtx b -> Map.Map b Typ -> Stmt b -> LintM (Stmt b)
lintStmt genSym linProj inferCtx tyCtx stmt =
    do stmt' <- Lin.runLinStmt genSym linProj stmt
       let stmt'' = canonizeStmt stmt'
       stmt''' <- (liftIO $ Tc.runTcStmt inferCtx tyCtx stmt'') >>= hoistExcept
       return stmt'''


lintDecl :: (TypedVar b Typ) => GenSym -> Bool -> InferCtx b -> Decl b -> LintM (Decl b)
lintDecl genSym linProj inferCtx decl =
    do decl' <- Lin.runLinDecl genSym linProj decl
       let decl'' = decl' { f_body = canonizeStmt (f_body decl') }
       decl''' <- (liftIO $ Tc.runTcDecl inferCtx decl'') >>= hoistExcept
       return decl'''

              
-----------------------------------
-- == Top-level

runLintStmt :: (TypedVar b Typ) => CompInfo -> Bool -> InferCtx b -> Map.Map b Typ -> Stmt b -> CompM (Stmt b)
runLintStmt cinfo linProj inferCtx openCtx stmt =
    do debugM "Low.LintLow" $ "LowPP/LowMM (Input) linProj: " ++ show linProj ++ "\n" ++ pprShow stmt
       stmt' <- lintStmt (getGenSym cinfo) linProj inferCtx openCtx stmt
       debugM "Low.LintLow" $ "LowPP/LowMM (Output):\n" ++ pprShow stmt'
       return stmt'


runLintDecl :: (TypedVar b Typ) => CompInfo -> Bool -> InferCtx b -> Decl b -> CompM (Decl b)
runLintDecl cinfo linProj inferCtx decl =
    do debugM "Low.LintLow" $ "LowPP/LowMM (Input) linProj: " ++ show linProj ++ "\n" ++ pprShow decl
       decl' <- lintDecl (getGenSym cinfo) linProj inferCtx decl
       debugM "Low.LintLow" $ "LowPP/LowMM (Output):\n" ++ pprShow decl'
       return decl'


runLintDecl' :: (TypedVar b Typ) => CompInfo -> Bool -> InferCtx b -> Decl b -> IO (Either String (Decl b))
runLintDecl' cinfo linProj inferCtx decl =
    runExceptT (lintDecl (getGenSym cinfo) linProj inferCtx decl)

               
runLintDeclTyVar' :: CompInfo -> Bool -> InferCtx (TVar Typ) -> Decl (TVar Typ) -> IO (Either String (Decl (TVar Typ)))
runLintDeclTyVar' = runLintDecl'
