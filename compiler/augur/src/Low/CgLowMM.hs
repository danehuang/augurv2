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

module Low.CgLowMM (runCgLowMM) where

import Debug.Trace
    
import Compile.CompData
import Core.CoreSyn
import Low.MemLow
import Low.ProjLow
import Low.LowSyn
import qualified Low.LowXXSyn as LX
    

----------------------------------------------------------------------
-- = CgLowpm Description
{-| [Note]

Input: code produced by middle-end
Output: Low M.Prim (TVar Typ) code

Low P.Prim (TVar Typ)   (linearized exp, types)
          | (lower, should be called mem...)
Low M.Prim (TVar Typ)   (linearized exp, types, allocs)
          | (proj)
Low M.Prim (TVar Typ)   (linearized exp, types, simpl proj, allocs)

-}


-----------------------------------
-- == Types and operations

type CgM = CompM


    
-----------------------------------
-- == Transformations

cgLowMM :: CompInfo -> CompOpt -> InferCtx TyId -> LX.LowPP TyId -> CgM (LX.LowMM TyId)
cgLowMM cinfo copt inferCtx lowppDecl =
    do LX.LowMM (LX.LowXX shpCtx' useAux' decl') <- runLowerDecl cinfo copt inferCtx lowppDecl       
       decl'' <- runProjDecl cinfo copt inferCtx decl'
       return $ LX.LowMM (LX.LowXX shpCtx' useAux' decl'')


              
-----------------------------------
-- == Top-level

runCgLowMM :: CompInfo -> CompOpt -> InferCtx TyId -> LX.LowPP TyId -> CompM (LX.LowMM TyId)
runCgLowMM cinfo copt inferCtx decl =
    cgLowMM cinfo copt inferCtx decl

