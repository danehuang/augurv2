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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AstUtil.Pretty where

import qualified Data.Map as Map
import Text.PrettyPrint


----------------------------------------------------------------------
-- = Pretty Description
{-| [Note]

Classes for pretty printing.

-}


-----------------------------------
-- == Classes

class Pretty a where
    ppr :: a -> Doc

    pprLvl :: Int -> a -> Doc
    pprLvl i = (nest i) . ppr

    pprShow :: a -> String
    pprShow = render . ppr
    
              
-----------------------------------
-- == Instances
              
instance Pretty () where
    ppr _ = text "()"

instance Pretty Int where
    ppr = int
          
instance Pretty String where
     ppr = text

instance (Pretty a, Pretty b) => Pretty (a, b) where
    ppr (x, y) = parens (ppr x <> comma <+> ppr y)

instance (Pretty a) => Pretty (Maybe a) where
    ppr (Just x) = text "Just" <+> ppr x
    ppr Nothing = text "Nothing"
                 
instance (Pretty a, Pretty b) => Pretty (Either a b) where
    ppr (Left l) = hang (text "Left") 2 (ppr l)
    ppr (Right r) = hang (text "Right") 2 (ppr r)

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    ppr = sepBy commasp . Map.toList
          
          
-----------------------------------
-- == Combinators
               
sepBy :: (Pretty a) => Doc -> [a] -> Doc
sepBy s docs = hcat (punctuate s (map ppr docs))

sepBy' :: Doc -> (a -> Doc) -> [a] -> Doc
sepBy' s f docs = hcat (punctuate s (map f docs))

commasp :: Doc
commasp = comma <> space

rendSepBy :: (Pretty a) => Doc -> [a] -> String
rendSepBy s docs = render (sepBy s docs)

pprShowLs :: (Pretty a) => [a] -> String
pprShowLs = rendSepBy commasp

pprShowLs' :: (Pretty a) => [a] -> String
pprShowLs' xs = render (vcat (map ppr xs))
