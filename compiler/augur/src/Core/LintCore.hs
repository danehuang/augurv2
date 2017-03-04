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

module Core.LintCore where

import Control.Monad.Except
import Debug.Trace

    
import AstUtil.Fresh
import AstUtil.Var
import Compile.CompData
import Core.CoreSyn
import Core.DensSyn
import Core.KernSyn
import Core.CoreTySyn
import qualified Core.LinCore as Lin
import qualified Core.TcCore as Tc
    

----------------------------------------------------------------------    
-- = LintCore description
{-| [Note]

"Lint" a Core program, i.e., normalize and check.

-}


-----------------------------------
-- == Types and operations

type LintM = ExceptT String IO

    
-----------------------------------
-- == Transformations
    
lintFn :: (TypedVar b Typ) => GenSym -> ModDecls b -> Fn b -> LintM (Fn b)
lintFn genSym modDecls fn =
    do fn' <- Lin.runLinFn genSym fn
       fn''' <- (liftIO $ Tc.runTcFn modDecls fn') >>= hoistExcept
       return fn'''


lintKernU :: (TypedVar b Typ) => GenSym -> ModDecls b -> KernU b -> LintM (KernU b)
lintKernU genSym modDecls k =
    do k' <- Lin.runLinKernU genSym k
       k'' <- (liftIO $ Tc.runTcKernU modDecls k') >>= hoistExcept
       return k''
              
              
-----------------------------------
-- == Top-level

runLintFn :: (TypedVar b Typ) => CompInfo -> ModDecls b -> Fn b -> CompM (Fn b)
runLintFn cinfo modDecls fn =
    lintFn (getGenSym cinfo) modDecls fn


runLintFn' :: (TypedVar b Typ) => CompInfo -> ModDecls b -> Fn b -> IO (Either String (Fn b))
runLintFn' cinfo modDecls fn =
    runExceptT (lintFn (getGenSym cinfo) modDecls fn)
               

runLintKernU :: (TypedVar b Typ) => CompInfo -> ModDecls b -> KernU b -> CompM (KernU b)
runLintKernU cinfo modDecls k =
    lintKernU (getGenSym cinfo) modDecls k
               
               
runLintKernU' :: (TypedVar b Typ) => CompInfo -> ModDecls b -> KernU b -> IO (Either String (KernU b))
runLintKernU' cinfo modDecls k =
    runExceptT (lintKernU (getGenSym cinfo) modDecls k)
