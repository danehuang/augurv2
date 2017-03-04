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

module Low.LowXXSyn where

import Text.PrettyPrint

import AstUtil.Pretty
import qualified Low.LowShpSyn as S
import qualified Low.LowSyn as L
    
----------------------------------------------------------------------
-- = LowXXSyn description
{-| [Note]

Contains LowXX syntax, which augments LowPP or LowMM with additional
information.

-}  


-----------------------------------
-- == Syntax

data LowXX b = LowXX { getGlobs :: S.ShpCtx b  -- ^ Global variables
                     , useMcmcPropSt :: Bool   -- ^ Uses MCMC proposal state?
                     , getDecl :: L.Decl b     -- ^ The declaration
                     }

newtype LowPP b = LowPP { unLowPP :: LowXX b }

newtype LowMM b = LowMM { unLowMM :: LowXX b }


-----------------------------------
-- == Instances

    
instance (Pretty b) => Pretty (LowPP b) where
    ppr (LowPP (LowXX shpCtx useProp decl)) =
        vcat [ text "globals" <+> ppr shpCtx, f useProp, ppr decl ]
        where
          f False = text "prop = False"
          f True = text "prop = True"
                  
instance (Pretty b) => Pretty (LowMM b) where
    ppr (LowMM (LowXX shpCtx useProp decl)) =
        vcat [ text "globals" <+> ppr shpCtx, f useProp, ppr decl ]
        where
          f False = text "prop = False"
          f True = text "prop = True"
