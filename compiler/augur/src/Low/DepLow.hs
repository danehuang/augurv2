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

module Low.DepLow where

import qualified Data.Set as Set
import Debug.Trace
    
import AstUtil.Var
import AstUtil.VarOp
import Core.CoreTySyn
import Low.LowSyn


----------------------------------------------------------------------
-- = DepLow Description
{-| [Note]

Compute dependency graph for Low program.

-}

        
-----------------------------------
-- == Dependencies

-- Expressions have no binding ...
genExp :: (TypedVar b Typ) => Exp b -> Set.Set b
genExp e = fvs e

           
genExps :: (TypedVar b Typ) => [Exp b] -> Set.Set b
genExps es =
    foldl (\acc gen -> gen `Set.union` acc) Set.empty (map genExp es)

          
genGen :: (TypedVar b Typ) => Gen b -> Set.Set b
genGen (Until e1 e2) = genExp e1 `Set.union` genExp e2

                       
gkStmt :: (TypedVar b Typ) => Stmt b -> (Set.Set b, Set.Set b)
gkStmt Skip = (Set.empty, Set.empty)
gkStmt (Exp e) = (genExp e, Set.empty)
gkStmt (Assign x e) = (genExp e, Set.singleton x)
gkStmt (Store x es _ e) =
    (Set.singleton x `Set.union` genExp e `Set.union` genExps es, Set.empty)
gkStmt (Seq s1 s2) =
    let (g1, k1) = gkStmt s1
        (g2, k2) = gkStmt s2
    in
      (g1 `Set.union` (g2 `Set.difference` k1), k2)
gkStmt (If e s1 s2) =
    let (g1, k1) = gkStmt s1
        (g2, k2) = gkStmt s2
    in
      (genExp e `Set.union` g1 `Set.union` g2, k1 `Set.union` k2)
gkStmt (Loop _ x gen s) =
    let (g1, k1) = gkStmt s
    in
      (g1 `Set.union` genGen gen, Set.insert x k1)
gkStmt (MapRed acc x gen s e) =
    let (g1, k1) = gkStmt s
    in
      (genGen gen `Set.union` g1 `Set.union` genExp e, Set.insert x (Set.insert acc k1))
       
