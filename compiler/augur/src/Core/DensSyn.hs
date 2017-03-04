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

module Core.DensSyn where

import Data.Foldable (Foldable)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable (Traversable)
import Text.PrettyPrint
import Data.Maybe

import AstUtil.AlphaEquiv
import AstUtil.Pretty
import AstUtil.Var
import AstUtil.VarOp
import Comm.DistSyn
import Low.LowpPrimSyn

   
----------------------------------------------------------------------
-- = DensSyn Description
{-| [Note]

Syntax for densities.

-}


-----------------------------------
-- == Syntax

data Fn b = Dens Dist (Exp b) [Exp b]
          | Ind (Fn b) [IndCond b]
          | Let b (Exp b) (Fn b)
          | Prod (Fn b) (Fn b)
          | Pi b (Gen b) (Fn b)
            deriving (Show, Functor, Foldable, Traversable)

newtype DensPt b = DensPt { unDensPt :: (b, [b]) }
    deriving (Show, Functor, Foldable, Traversable)
                     
data IndCond b = CatCond b (Exp b)
                 deriving (Show, Functor, Foldable, Traversable)
                          
data Gen b = Until (Exp b) (Exp b)
             deriving (Show, Functor, Foldable, Traversable)
                          
data Exp b = Var b
           | Lit Lit
           | DistOp Dop Dist [Exp b]
           | Call CallExp [Exp b]
           | Proj (Exp b) [Exp b]
               deriving (Show, Functor, Foldable, Traversable)

data CallExp = FnId Name
             | PrimId Prim
               deriving (Show)
                        
data Lit = Int Int
         | Real Double
           deriving (Show)


-----------------------------------
-- == Instances


-- === Pretty

instance (Pretty b) => Pretty (Fn b) where
    ppr (Dens dist ept es) =
        ppr dist <> parens (ppr ept <+> semi <+> sepBy commasp es)
    ppr (Ind fn cond) =
        brackets (ppr fn <+> text "|" <+> sepBy commasp cond)
    ppr (Let x e fn) =
        vcat [ text "let" <+> ppr x <+> text "=" <+> ppr e <+> text "in"
             , ppr fn ]
    ppr (Prod fn1 fn2) = vcat [ ppr fn1, ppr fn2 ]
    ppr (Pi x gen fn) =
        vcat [ hang (text "Pi" <> parens (ppr x <+> text "<-" <+> ppr gen) <+> lbrace) 2 (ppr fn), rbrace ]

instance (Pretty b) => Pretty (DensPt b) where
    ppr (DensPt (x, idxs)) =
        case idxs of
          [] -> ppr x
          _ -> ppr x <> brackets (sepBy commasp idxs)
             
instance (Pretty b) => Pretty (IndCond b) where
    ppr (CatCond x e) = ppr x <+> text "=" <+> ppr e

instance (Pretty b) => Pretty (Gen b) where
    ppr (Until e1 e2) = ppr e1 <+> text "until" <+> ppr e2

instance (Pretty b) => Pretty (Exp b) where
    ppr (Var x) = ppr x
    ppr (Lit lit) = ppr lit
    ppr (DistOp dop dist es) =
        ppr dist <> text "." <> ppr dop <> parens (sepBy commasp es)
    ppr (Call ce es) = ppr ce <> parens (sepBy commasp es)
    ppr (Proj e es) = ppr e <> brackets (sepBy commasp es)

instance Pretty CallExp where
    ppr (FnId name) = ppr name
    ppr (PrimId prim) = ppr prim

instance Pretty Lit where
    ppr (Int i) = int i
    ppr (Real d) = double d

                   
-- === FreeVar

instance (Ord b) => FreeVar b (Fn b) where
    fvs (Dens _ pt es) = fvs pt `Set.union` fvsLs es
    fvs (Ind fn conds) = fvs fn `Set.union` fvsLs conds
    fvs (Let x e fn) = fvs e `Set.union` (x `Set.delete` fvs fn)
    fvs (Prod fn1 fn2) = fvs fn1 `Set.union` fvs fn2
    fvs (Pi x gen fn) = fvs gen `Set.union` (x `Set.delete` fvs fn)

instance (Ord b) => FreeVar b (DensPt b) where
    fvs (DensPt (x, idxs)) = Set.fromList (x : idxs)
                        
instance (Ord b) => FreeVar b (IndCond b) where
    fvs (CatCond x e) = Set.singleton x `Set.union` fvs e
                        
instance (Ord b) => FreeVar b (Gen b) where
    fvs (Until e1 e2) = fvs e1 `Set.union` fvs e2

instance (Ord b) => FreeVar b (Exp b) where
    fvs (Var x) = Set.singleton x
    fvs (Lit _) = Set.empty
    fvs (DistOp _ _ es) = fvsLs es
    fvs (Call _ es) = fvsLs es
    fvs (Proj e es) = fvs e `Set.union` fvsLs es


-- === Substitutable

instance (Ord b) => Substitutable b (Fn b) (Exp b) where
    substP p x term = sub
        where
          sub (Dens dist pt es) = Dens dist (substP p x term pt) (map (substP p x term) es)
          sub (Ind fn conds) = Ind (sub fn) (map (substP p x term) conds)
          sub (Let y e fn)
              | x `p` y = Let y (substP p x term e) fn
              | otherwise = Let y (substP p x term e) (sub fn)
          sub (Prod fn1 fn2) = Prod (sub fn1) (sub fn2)
          sub (Pi y gen fn)
              | x `p` y = Pi y (substP p x term gen) fn
              | otherwise = Pi y (substP p x term gen) (sub fn)
                            
instance (Ord b) => Substitutable b (IndCond b) (Exp b) where
    substP p x term = sub
        where
          sub (CatCond y e) = CatCond y (substP p x term e)
                            
instance (Ord b) => Substitutable b (Gen b) (Exp b) where
    substP p x term = sub
        where
          sub (Until e1 e2) = Until (substP p x term e1) (substP p x term e2)
    
instance (Ord b) => Substitutable b (Exp b) (Exp b) where
    substP p x term = sub
        where
          sub (Var y) | x `p` y = term
                      | otherwise = Var y
          sub (Lit lit) = Lit lit
          sub (Call ce es) = Call ce (map sub es)
          sub (DistOp dop dist es) = DistOp dop dist (map sub es)
          sub (Proj e es) = Proj (sub e) (map sub es)

                            
-- === AlphaEquiv
     
instance (Ord b) => AlphaEquiv (Exp b) where
    (=\=) (Var x) (Var x') = x == x'
    (=\=) (Lit lit) (Lit lit') = lit =\= lit'
    (=\=) (Call ce es) (Call ce' es') = ce =\= ce' && es =\= es'
    (=\=) (DistOp dop dist es) (DistOp dop' dist' es') =
        dop == dop' && dist == dist' && es =\= es'    
    (=\=) (Proj e es) (Proj e' es') =
        e =\= e' && es =\= es'
    (=\=) _ _ = False

instance AlphaEquiv CallExp where
    (=\=) (FnId f1) (FnId f2) = f1 == f2
    (=\=) (PrimId prim1) (PrimId prim2) = prim1 == prim2
    (=\=) _ _ = False
                
instance AlphaEquiv Lit where
    (=\=) (Int i1) (Int i2) = i1 == i2
    (=\=) (Real d1) (Real d2) =
        -- Double trickiness because NaN \neq NaN?
        d1 == d2
    (=\=) _ _ = False
                
instance (Ord b) => AlphaEquiv (Gen b) where
    (=\=) (Until e1 e2) (Until e1' e2') =
        e1 =\= e1' && e2 =\= e2'

instance (Ord b) => AlphaEquiv (DensPt b) where
    (=\=) (DensPt (x, idxs)) (DensPt (x', idxs')) =
        x == x' && idxs == idxs'

instance (Ord b) => AlphaEquiv (IndCond b) where
    (=\=) (CatCond x e) (CatCond x' e') =
        x == x' && e =\= e'
          
instance (Ord b) => AlphaEquiv (Fn b) where
    (=\=) (Dens dist ept es) (Dens dist' ept' es') =
        dist == dist' && ept =\= ept' && es =\= es'
    (=\=) (Ind fn cond) (Ind fn' cond') =
        fn =\= fn' && cond =\= cond'
    (=\=) (Let x e fn) (Let x' e' fn') =
        (subst x e fn) =\= (subst x' e' fn')
    (=\=) (Prod fn1 fn2) (Prod fn1' fn2') =
        -- Should normalize 
        fn1 =\= fn1' && fn2 =\= fn2' 
    (=\=) (Pi x gen f) (Pi x' gen' f') =
        gen =\= gen' && f =\= (subst x (Var x') f')
    (=\=) _ _ = False
             

                            
-----------------------------------
-- == Operations on Syntax

dirac :: b -> [b] -> Fn b
dirac x [] = Dens Dirac (Var x) [ Var x ]
dirac x idxs = Dens Dirac pt [ pt ]
    where
      pt = Proj (Var x) (map Var idxs)

           
isDirac :: Fn b -> Bool
isDirac (Dens dist _ _) =
    case dist of
      Dirac -> True
      _ -> False
isDirac (Let _ _ fn) = isDirac fn
isDirac (Ind fn _) = isDirac fn
isDirac (Prod _ _) = error $ "[DensSyn] | Shouldn't call isDirac with unfactored function"
isDirac (Pi _ _ fn) = isDirac fn

                      
isDiscFn :: Fn b -> Bool
isDiscFn (Dens dist _ _) =
    case dist of
      Dirac -> False
      _ -> isDisc dist
isDiscFn (Let _ _ fn) = isDiscFn fn
isDiscFn (Ind fn _) = isDiscFn fn
isDiscFn (Prod _ _) = error $ "[DensSyn] | Shouldn't call isDiscFn with unfactored function"
isDiscFn (Pi _ _ fn) = isDiscFn fn
                      
           
densPtToExp :: (BasicVar b) => DensPt b -> Exp b
densPtToExp (DensPt (x, idxs)) =
    case idxs of
      [] -> Var x
      _ -> Proj (Var x) (map Var idxs)

mkDensPt' :: (BasicVar b) => b -> [b] -> Exp b
mkDensPt' x xs =
    case xs of
      [] -> Var x
      _ -> Proj (Var x) (map Var xs)


mkDensPt :: (BasicVar b) => b -> [Exp b] -> Exp b
mkDensPt x es =
    case es of
      [] -> Var x
      _ -> Proj (Var x) es


densPtVar :: (BasicVar b) => Exp b -> b
densPtVar (Var x) = x
densPtVar (Proj (Var x) _) = x
densPtVar e = error $ "[DensSyn] @densPtVar | Cannot project " ++ pprShow e

densPtTy :: (TypedVar b t) => Exp b -> t
densPtTy pt = fromJust (getType (densPtVar pt))
              
densPtIdx :: (BasicVar b) => Exp b -> [b]
densPtIdx (Var _) = []
densPtIdx (Proj _ es) = map (\e -> case e of
                                     Var x -> x
                                     _ -> error $ "dang bro") es
densPtIdx e = error $ "[DensSyn] @densPtIdx | Cannot project " ++ pprShow e
                                         
densPtIdx' :: (BasicVar b) => Exp b -> [Exp b]
densPtIdx' = map Var . densPtIdx
              
gatherGen :: (BasicVar b) => Fn b -> [(b, Gen b)]
gatherGen (Dens _ _ _) = []
gatherGen (Ind fn _) = gatherGen fn
gatherGen (Let x e fn) = gatherGen (subst x e fn)
gatherGen (Prod fn1 fn2) = gatherGen fn1 ++ gatherGen fn2
gatherGen (Pi x gen fn) = (x, gen) : gatherGen fn

                          
gatherDensPt :: (BasicVar b) => Fn b -> Exp b
gatherDensPt (Dens _ pt _) = pt
gatherDensPt (Ind fn _) = gatherDensPt fn
gatherDensPt (Let _ _ fn) = gatherDensPt fn
gatherDensPt (Prod _ _) = error $ "[DensSyn] @gatherDensPt | Shouldn't happen"
gatherDensPt (Pi _ _ fn) = gatherDensPt fn


gatherDensArgs :: (BasicVar b) => Fn b -> [Exp b]
gatherDensArgs (Dens _ _ es) = es
gatherDensArgs (Ind fn _) = gatherDensArgs fn
gatherDensArgs (Let _ _ fn) = gatherDensArgs fn
gatherDensArgs (Prod _ _) = error $ "[DensSyn] @gatherDensArgs | Shouldn't happen"
gatherDensArgs (Pi _ _ fn) = gatherDensArgs fn
                           
                           
newtype Gen' b = Gen' { unGen :: (b, Gen b) }

instance (Ord b) => Eq (Gen' b) where
    (==) (Gen' (x, gen)) (Gen' (x', gen')) =
        x == x' && gen =\= gen' 

              
gatherGen' :: (BasicVar b) => Fn b -> [Gen' b]
gatherGen' = map Gen' . gatherGen

             
containsGen :: (BasicVar b) => Gen b -> Fn b -> Bool
containsGen gen fn =
    any (\gen' -> gen =\= gen') (map snd (gatherGen fn))

containsGen' :: (BasicVar b) => Gen' b -> Fn b -> Bool
containsGen' gen fn =
    any (\gen' -> gen == gen') (gatherGen' fn)

        
containsPt :: (BasicVar b) => b -> Fn b -> Bool
containsPt v = go
    where
      go (Dens _ pt _) =
          case pt of
            Var x -> x == v
            Proj (Var x) _ -> x == v
            e -> error $ "[DensSyn] @containsPt | Shouldn't happen " ++ pprShow e
      go (Ind fn _) = go fn
      go (Let _ _ fn) = go fn
      go (Prod fn1 fn2) = go fn1 || go fn2
      go (Pi _ _ fn) = go fn
                      
prodFn :: [Fn b] -> Fn b
prodFn [] = error $ "[DensSyn] @prodFn | Cannot call with empty list." -- (Dens Dirac (Lit (Int 0)) [])
prodFn fns = foldl (\acc s -> Prod acc s) (head fns) (tail fns)


gatherCatCstr :: (BasicVar b) => Fn b -> [IndCond b]
gatherCatCstr Dens{} = []
gatherCatCstr (Ind fn conds) =
    filter (\cond -> case cond of
                       CatCond{} -> True) conds ++ gatherCatCstr fn
gatherCatCstr (Let _ _ fn) = gatherCatCstr fn
gatherCatCstr (Prod fn1 fn2) = gatherCatCstr fn1 ++ gatherCatCstr fn2
gatherCatCstr (Pi _ _ fn) = gatherCatCstr fn
                     
             
gatherCat :: (BasicVar b) => Fn b -> [Exp b]
gatherCat (Dens dist pt _) =
    case dist of
      Categorical -> [pt]
      _ -> []
gatherCat (Ind fn _) = gatherCat fn
gatherCat (Let x e fn) = gatherCat (subst x e fn)
gatherCat (Prod fn1 fn2) = gatherCat fn1 ++ gatherCat fn2
gatherCat (Pi _ _ fn) = gatherCat fn
         
gatherCat' :: (BasicVar b) => Fn b -> Map.Map b Int
gatherCat' fn =
    Map.fromList (map (\pt -> (densPtVar pt, length (densPtIdx pt))) (gatherCat fn))


             
-----------------------------------
-- == Substitution
              
substAExp :: (BasicVar b) => Exp b -> Exp b -> Exp b -> Exp b
substAExp eSubst eFor = sub
    where
      sub eIn | eFor =\= eIn = eSubst
              | otherwise = eIn

substAExpGen :: (BasicVar b) => Exp b -> Exp b -> Gen b -> Gen b
substAExpGen eSubst eFor = sub
    where
      sub (Until e1 e2) = Until (substAExp eSubst eFor e1) (substAExp eSubst eFor e2)

substAExpIndCond :: (BasicVar b) => Exp b -> Exp b -> IndCond b -> IndCond b
substAExpIndCond eSubst eFor = sub
    where
      sub (CatCond x e) = CatCond x (substAExp eSubst eFor e)
                              
substAExpFn :: (BasicVar b) => Exp b -> Exp b -> Fn b -> Fn b
substAExpFn eSubst eFor = sub
    where
      sub (Dens dist pt es) =
          Dens dist (substAExp eSubst eFor pt) (map (substAExp eSubst eFor) es)
      sub (Ind fn conds) =
          Ind (sub fn) (map (substAExpIndCond eSubst eFor) conds)
      sub (Let x e fn) =
          Let x (substAExp eSubst eFor e) (sub fn)
      sub (Prod fn1 fn2) =
          Prod (sub fn1) (sub fn2)
      sub (Pi x gen fn) =
          Pi x (substAExpGen eSubst eFor gen) (sub fn)


