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

{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

module Core.DepCore where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Set as Set
import Debug.Trace
    
import AstUtil.Var
import AstUtil.VarOp
import Core.DensSyn
import qualified Core.Graph as G
import Core.CoreTySyn
import qualified Core.RwCore as RW
    

----------------------------------------------------------------------
-- = DepCore Description
{-| [Note]

Compute dependency graph for Core program.

Dens (TVar Typ)        (simpl expr, types)
       |
Graph (TVar Typ)

-}

    

-----------------------------------
-- == Types and operations

type DepM b m = ReaderT b (WriterT [(b, b)] m)


-----------------------------------
-- == Dependencies


depIndCond :: (TypedVar b Typ) => IndCond b -> [(b, b)]
depIndCond (CatCond x e) =
    map (\y -> (x, y)) (Set.toList (fvs e)) -- TODO: huh!? don't understand anymore

    
depFn :: (TypedVar b Typ) => Fn b -> [(b, b)]
depFn (Dens _ _ _) = []
depFn (Ind fn conds) =
    depFn fn ++ concat (map depIndCond conds)
depFn (Let x e fn) =
    map (\y -> (x, y)) (Set.toList (fvs e)) ++ depFn fn
depFn (Prod fn1 fn2) =
    depFn fn1 ++ depFn fn2
depFn (Pi x gen fn) =
    map (\y -> (x, y)) (Set.toList (fvs gen)) ++ depFn fn
       
        
depFnG :: Fn (TVar Typ) -> G.Graph (TVar Typ)
depFnG fn = G.mkGraph (depFn fn)


            
-----------------------------------
-- == Dependencies
            

depTopLevel :: Fn (TVar Typ) -> G.Graph (TVar Typ)
depTopLevel fn =
    let fns = RW.unfactor fn
        adjLs = map mkAdjLs fns
    in
      G.mkGraph (concat adjLs)
    where
      mkAdjLs fn' =
          let deps = filter (isModDecl . idKind) (Set.toList (fvs fn'))
              v = densPtVar (gatherDensPt fn')
          in
            map (\v' -> (v', v)) deps

