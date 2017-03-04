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

module Comm.TypSyn where

import Text.PrettyPrint

import AstUtil.Pretty


----------------------------------------------------------------------    
{-| [Note]

Common type syntax.
-}

    
data Typ = UnitTy
         | IntTy
         | RealTy
         | VecTy Typ
         | MatTy Typ
         | ArrTy [Typ] Typ
           deriving (Eq, Show)


-----------------------------------
-- == Instances

instance Pretty Typ where
    ppr UnitTy = text "unit"
    ppr IntTy = text "int"
    ppr RealTy = text "real"
    ppr (VecTy t) = text "vec<" <> ppr t <> text ">"
    ppr (MatTy t) = text "mat<" <> ppr t <> text ">"
    ppr (ArrTy ts t) = parens (sepBy (text "*") ts <+> text "->" <+> ppr t)
