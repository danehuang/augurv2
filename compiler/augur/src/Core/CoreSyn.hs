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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts #-}

module Core.CoreSyn where

import qualified Data.Map as Map
    
import AstUtil.Var
import Core.CoreTySyn
import Core.DensSyn
import Core.KernSyn


----------------------------------------------------------------------
-- = CoreSyn Description
{-| [Note]

Core syntax for
1) Top-level
2) Model
3) Inference

-}



-----------------------------------
-- == Syntax

data Core b =
    Core { core_params :: ModDecls b
         , core_model :: Fn b
         , core_infer :: Maybe (KernU b) }


data InferCtx b =
    IC { ic_modBlkCtx :: ModBlkCtx b
       , ic_dupCtx :: ModParamDupCtx b
       , ic_modDecls :: ModDecls b
       , getIdxVar :: b }
    
type ModBlkCtx b = Map.Map b [b]
    
type ModParamDupCtx b = Map.Map b b
    
type ModDecls b = [ModDecl b]
type ModDecl b = (IdKind, b, Typ)

getModDeclIds :: ModDecls b -> [b]
getModDeclIds = map (\(_, x, _) -> x)

getModHyperIds :: ModDecls b -> [b]
getModHyperIds = 
    map (\(_, x, _) -> x) . filter (\(ik, _, _) -> isModHyper ik)
    
getModParamIds :: ModDecls b -> [b]
getModParamIds =
    map (\(_, x, _) -> x) . filter (\(ik, _, _) -> isModParam ik)

getModDataIds :: ModDecls b -> [b]
getModDataIds =
    map (\(_, x, _) -> x) . filter (\(ik, _, _) -> isModData ik)
        
getModAuxIds :: ModDecls b -> [b]
getModAuxIds =
    map (\(_, x, _) -> x) . filter (\(ik, _, _) -> isModAux ik)

