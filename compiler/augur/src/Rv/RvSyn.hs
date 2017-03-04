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

module Rv.RvSyn where

import Text.PrettyPrint
    
import AstUtil.Pretty
import AstUtil.Var
import Core.DensSyn
import Core.CoreTySyn
    
----------------------------------------------------------------------
-- = RvSyn Description
{-| [Note]

Random variable surface syntax.

-}


data Model =
    Model { model_params :: [(IdKind, String, Typ)]
          , model_decls :: [Decl]
          }
    deriving (Show)
    
data Decl =
    Decl { decl_name :: String
         , decl_ty :: Typ
         , decl_kind :: IdKind
         , decl_grid :: [(String, Gen String)]
         , decl_body :: Exp String
         }
    deriving (Show)

                   
-----------------------------------
-- == Instances

instance Pretty Decl where
    ppr (Decl name ty kind grid body) =
        ppr kind <+> text name <> brackets (sepBy' commasp pprGrid grid) <+> text "~" <+> ppr body <+> text "for" <+> (sepBy' commasp pprGrid grid)
        where
          pprGrid (x, gen) = text x <+> text "<-" <+> ppr gen
