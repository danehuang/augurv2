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

module Core.CgDet
    ( runDetFn
    , runDetFn'
    , detName ) where

import qualified Data.Map as Map
    
import AstUtil.Var
import Core.DensSyn
import Comm.DistSyn
import qualified Low.LowSyn as L
import qualified Low.LowXXSyn as LX
import Core.CoreSyn
import Core.CoreTySyn
import qualified Low.LintLow as Lint
import Compile.CompData
import Core.CgCore

    
----------------------------------------------------------------------
-- = CgDet Description
{-| [Note]

-}



-----------------------------------
-- == Types and operations
    
type DetM = CompM


-----------------------------------
-- == Transformation
    
detFnBody :: (TypedVar b Typ) => Fn b -> DetM (L.Stmt b)
detFnBody (Dens dist pt es) =
    case dist of
      Dirac ->
          do let x = densPtVar pt
                 idxs = map cgExp (densPtIdx' pt)
                 e = cgExp (es !! 0)
             return $ L.Store x idxs L.Update e
      _ -> error $ "[CgDet] | Shouldn't happen"
detFnBody (Ind fn conds) =
    do let conds' = map cgIndCond conds
       s <- detFnBody fn
       return $ L.If (andExp conds') s L.Skip
detFnBody (Let x e fn) =
    do s <- detFnBody fn
       return $ L.Seq (L.Assign x (cgExp e)) s
detFnBody (Prod _ _) = error $ "[CgDet] | Shouldn't happen"
detFnBody (Pi x gen fn) =
    do let gen' = cgGen gen
       s <- detFnBody fn
       return $ L.Loop L.Parallel x gen' s


detFnDecl :: (TypedVar b Typ) => Name -> Fn b -> DetM (L.Decl b)
detFnDecl name fn =
    do body <- detFnBody fn
       let params = []
           allocs = []
       return $ L.Fun name params allocs body Nothing UnitTy


-----------------------------------
-- == Top-level

detName :: (TypedVar b Typ) => b -> Name
detName v_mod = mkCompName "det" (varName v_mod)

runDetFn :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> CompM (L.Decl b)
runDetFn cinfo copt inferCtx v_mod fn =
    do v <- detFnDecl (detName v_mod) fn
       runLint copt v (Lint.runLintDecl cinfo False inferCtx)

runDetFn' :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> CompM (LX.LowPP b)
runDetFn' cinfo copt inferCtx v_mod fn =
    do v <- runDetFn cinfo copt inferCtx v_mod fn
       -- TODO: Is this sound? when i update the current, should i also update the proposal?
       return $ LX.LowPP (LX.LowXX Map.empty False v) 
