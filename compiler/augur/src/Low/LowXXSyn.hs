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
                     , getCC :: CallConv       -- ^ Where we call this decl from
                     , projIdx :: [b]          -- ^ Project indices
                     , getDecl :: L.Decl b     -- ^ The declaration
                     }

newtype LowPP b = LowPP { unLowPP :: LowXX b }

newtype LowMM b = LowMM { unLowMM :: LowXX b }

data CallConv = HostCall { isHostCode :: Bool }
              | DevCall { getDynPar :: Bool }
                deriving (Show)

    
-----------------------------------
-- == Instances


instance (Pretty b) => Pretty (LowPP b) where
    ppr (LowPP (LowXX shpCtx useProp cc projIdx decl)) =
        vcat [ text "globals" <+> ppr shpCtx
             , f useProp
             , text "cc" <+> text "=" <+> ppr cc
             , ppr decl ]
        where
          f False = text "prop = False"
          f True = text "prop = True"
                  
instance (Pretty b) => Pretty (LowMM b) where
    ppr (LowMM (LowXX shpCtx useProp cc projIdx decl)) =
        vcat [ text "globals" <+> ppr shpCtx
             , f useProp
             , text "cc" <+> text "=" <+> ppr cc
             , ppr decl ]
        where
          f False = text "prop = False"
          f True = text "prop = True"

instance Pretty CallConv where
    ppr (HostCall hostCode) = if hostCode then text "hostCode" else text "hostCall"
    ppr (DevCall dynPar) = if dynPar then text "devDyn" else text "dev"
