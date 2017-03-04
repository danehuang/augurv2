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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Comm.Prim where

import Text.PrettyPrint
    
import AstUtil.Pretty


----------------------------------------------------------------------
-- = Prim Description
{-| [Note]

-}


-----------------------------------
-- == Types and operations

data DopMode = DM_Fn
             | DM_Mem
               deriving (Eq, Show)
                  
data PrimMode = PM_Fn
              | PM_Grad Int
                deriving (Show)

class (Pretty p) => Primitive p t | p -> t where
    -- getArgTy :: PrimMode -> p -> [t]
    -- getRetTy :: PrimMode -> p -> t
    -- getPrimTy ::  PrimMode -> p -> [t]
    getPrimTy :: DopMode -> PrimMode -> p -> [t]
    -- isNumeric :: p -> Bool
    isInfix :: p -> Bool
    -- isOverload :: p -> Bool

                         
-----------------------------------
-- == Instances
                                                  
instance Pretty PrimMode where
    ppr PM_Fn = text "fn"
    ppr (PM_Grad pos) = text "grad" <> text "_" <> ppr pos

instance Pretty DopMode where
    ppr DM_Fn = text "fn"
    ppr DM_Mem = text "mem"

