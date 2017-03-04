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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module AstUtil.VarOp where

import qualified Data.Set as Set


----------------------------------------------------------------------
-- = VarOp Description
{-| [Note]

Classes for variables.

-}
    

-----------------------------------
-- == Classes

class (Ord b) => DefUse b t | t -> b where
    def :: t -> Set.Set b
    use :: t -> Set.Set b

class (Ord b) => FreeVar b t | t -> b where
    fvs :: t -> Set.Set b

class (Ord b) => Substitutable b t u | t -> b, u -> b where
    substP :: (b -> b -> Bool) -> b -> u -> t -> t

    subst :: b -> u -> t -> t
    subst = substP (==)


-----------------------------------
-- == Operations

fvsLs :: (FreeVar b a) => [a] -> Set.Set b
fvsLs = foldl (\acc e' -> Set.union acc (fvs e')) Set.empty
