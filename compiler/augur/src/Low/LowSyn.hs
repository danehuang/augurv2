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

module Low.LowSyn where

import Data.Foldable (Foldable)
import qualified Data.Set as Set
import Data.Traversable (Traversable)
import Text.PrettyPrint

import AstUtil.Pretty
import AstUtil.Var
import AstUtil.VarOp
import Comm.DistSyn
import Comm.Prim
import Core.CoreTySyn
import qualified Low.LowpPrimSyn as P


----------------------------------------------------------------------
-- = LowSyn Description
{-| [Note]

Syntax for Low++ and Low-- AST. It is parameterized by primitives and binders.

== Low semantics

Binding forms:
1) Function parameters
2) Assign x e          (x = e)

Everything in Low is an object, even "base types" such as integers. For example,

1) (Var x) :: IntTy    translates to "read value from location named by x, i.e., !x"
2) Store x [] Inc x 1  translates to "increment value at location x by 1"

-}
    
type TyId = TVar Typ

newtype Prog b = Prog { unProg :: [Decl b] }

data Decl b =
    Fun { f_name :: Name
        , f_params :: [(b, Typ)]
        , f_allocs :: [b] -- This should be the GLOBALS that this function touches
        , f_body :: Stmt b
        , f_retExp :: Maybe (Exp b)
        , f_retTy :: Typ
        } deriving (Show, Functor, Foldable, Traversable)
    
data Stmt b = Skip
            | Exp (Exp b)
            | Assign b (Exp b)                  -- Only local variables
            -- | Alloc b Typ (Exp b)
            | Store b [Exp b] UpKind (Exp b)
            | Seq (Stmt b) (Stmt b)
            | If (Exp b) (Stmt b) (Stmt b)
            | Loop LoopKind b (Gen b) (Stmt b)
              
            -- x = mapred(i <- gen) { stmt; ret e }
            | MapRed b b (Gen b) (Stmt b) (Exp b)
              deriving (Show, Functor, Foldable, Traversable)

data LoopKind = Sequential
              | Parallel
              | AtomicPar
                deriving (Eq, Show)

data UpKind = Update
            | AtmInc
            | Inc
            | AtmMInc
            | MInc
              deriving (Eq, Show)

data Gen b = Until (Exp b) (Exp b)
           deriving (Show, Functor, Foldable, Traversable)
                    
data Exp b = Var b
           | Lit Lit
           | DistOp Dop DopMode Dist [Exp b]
           | Call (CallExp) [Exp b]
           | Proj (Exp b) [Exp b]
             deriving (Show, Functor, Foldable, Traversable)

data CallExp = FnId Name
             | PrimId DopMode PrimMode P.Prim
               deriving (Show)
                        
data Lit = Int Int
         | Real Double
           deriving (Show)


-----------------------------------
-- == Instances

instance (Pretty b) => Pretty (Prog b) where
    ppr prog = vcat (map ppr (unProg prog))

instance (Pretty b) => Pretty (Decl b) where
    ppr (Fun name params allocs body retExp retTy) =
        vcat [ hang (ppr name <> parens (sepBy' (comma <> space) pprParam params) <> parens (sepBy (comma <> space) allocs) <+> colon <+> ppr retTy <+> lbrace) 2 (vcat [ ppr body, pprRet retExp ]), rbrace ]
        where
          pprParam (x, ty) = ppr x <+> colon <+> ppr ty
          
          pprRet (Just e) = text "return" <+> ppr e <+> semi
          pprRet Nothing = empty
                           
instance (Pretty b) => Pretty (Stmt b) where
    ppr Skip = text "skip" <> semi
    ppr (Exp e) = ppr e <> semi
    ppr (Assign x e) = ppr x <+> text "=" <+> ppr e <> semi
    ppr (Store e es uk e') = ppr e <> idx <+> colon <> ppr uk <+> ppr e' <> semi
        where
          idx | length es == 0 = empty
              | otherwise = brackets (sepBy (comma <> space) es)              
    ppr (Seq s1 s2) = vcat [ ppr s1, ppr s2 ]
    ppr (If e s1 s2) =
        vcat [ hang (text "if" <> parens (ppr e) <+> lbrace) 2 (ppr s1), rbrace, hang (text "else" <+> lbrace) 2 (ppr s2), rbrace ]
    ppr (Loop lk x gen s) =
        -- ppr lk <+> text "loop" <> parens (ppr x <+> text "<-" <+> ppr gen) <+> braces (nest 2 (ppr s))
        vcat [ hang (ppr lk <+> text "loop" <> parens (ppr x <+> text "<-" <+> ppr gen) <+> lbrace) 2 (ppr s), rbrace ]
    ppr (MapRed acc x gen s e) = vcat [ hang (ppr acc <+> text "=" <+> text "mapred" <> parens (ppr x <+> text "<-" <+> ppr gen) <+> lbrace) 2 (vcat [ ppr s, text "return" <+> ppr e <+> semi ]), rbrace ]
           
instance Pretty LoopKind where
    ppr Sequential = text "seq"
    ppr Parallel = text "par"
    ppr AtomicPar = text "atmpar"
           
instance Pretty UpKind where
    ppr Update = text "="
    ppr AtmInc = text "@+="
    ppr Inc = text "+="
    ppr AtmMInc = text "@*="
    ppr MInc = text "*="
           
instance (Pretty b) => Pretty (Gen b) where
    ppr (Until e1 e2) = ppr e1 <+> text "until" <+> ppr e2
           
instance (Pretty b) => Pretty (Exp b) where
    ppr (Var x) = ppr x
    ppr (Lit lit) = ppr lit
    ppr (DistOp dop dm dist es) =
        case dm of
          DM_Fn -> ppr dist <> text "." <> ppr dop <> parens (sepBy commasp es)
          DM_Mem -> ppr dist <> text "M." <> ppr dop <> parens (sepBy commasp es)
    ppr (Call ce es) = ppr ce <> parens (sepBy commasp es)
{-
        case ce of
          FnId _ -> 
          PrimId prim ->
              if isInfix prim
              then ppr ce <> parens (sepBy comma es)
              else ppr ce <> parens (sepBy comma es)
-}
    ppr (Proj e es) = ppr e <> brackets (sepBy comma es)
           
instance Pretty (CallExp) where
    ppr (FnId name) = ppr name
    ppr (PrimId dm pm f) =
        case pm of
          PM_Fn -> ppr dm <> text "_" <> ppr pm <> text "_" <> ppr f
          PM_Grad arg ->  ppr dm <> text "_" <> ppr pm <> text "_" <> text "dot" <> text "_" <> int arg <> text "_" <> ppr f
           
instance Pretty Lit where
    ppr (Int i) = int i
    ppr (Real d) = double d
                              
                   


instance (Ord b) => FreeVar b (Gen b) where
    fvs (Until e1 e2) = fvs e1 `Set.union` fvs e2

instance (Ord b) => FreeVar b (Exp b) where
    fvs (Var x) = Set.singleton x
    fvs (Lit _) = Set.empty
    fvs (DistOp _ _ _ es) = fvsLs es
    fvs (Call _ es) = fvsLs es
    fvs (Proj e es) = fvs e `Set.union` fvsLs es

{-                      
instance (Ord b) => FreeVar b (Stmt b) where
    fvs Skip = Set.empty
    fvs (Exp e) = fvs e
    fvs (Assign _ e) = fvs e
    fvs (Store _ es _ e') = fvsLs es `Set.union` fvs e'
    fvs (Seq s1 s2) = fvs s1 `Set.union` fvs s2
    fvs (If e s1 s2) = fvs e `Set.union` fvs s1 `Set.union` fvs s2
    fvs (Loop _ x gen s) = (Set.delete x (fvs gen)) `Set.union` fvs s
    fvs (MapRed acc x gen s e) = Set.delete acc ((Set.delete x (fvs gen)) `Set.union` fvs s `Set.union` fvs e)
-}


instance (Ord b) => Substitutable b (Stmt b) (Exp b) where
    substP p x term = sub
        where
          sub Skip = Skip
          sub (Exp e) = Exp (substP p x term e)
          sub (Assign y e) =
              Assign y (substP p x term e)
          sub (Store y es uk e') =
              Store y (map (substP p x term) es) uk (substP p x term e')
          sub (Seq s1 s2) = Seq (sub s1) (sub s2)
          sub (If e s1 s2) = If (substP p x term e) (sub s1) (sub s2)
          sub (Loop lk y gen s)
              | x `p` y = Loop lk y (substP p x term gen) s
              | otherwise = Loop lk y (substP p x term gen) (sub s)
          sub (MapRed acc y gen s e)
              | x `p` y = MapRed acc y (substP p x term gen) s (substP p x term e)
              | otherwise = MapRed acc y (substP p x term gen) (sub s) (substP p x term e)
        
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
          sub (DistOp dop dm dist es) = DistOp dop dm dist (map sub es)
          sub (Proj e es) = Proj (sub e) (map sub es)
                            

instance Num (Exp b) where
    (+) e1 e2 = Call (PrimId DM_Fn PM_Fn P.Plus) [ e1, e2 ]
    (-) e1 e2 = Call (PrimId DM_Fn PM_Fn P.Minus) [ e1, e2 ]
    (*) e1 e2 = Call (PrimId DM_Fn PM_Fn P.Times) [ e1, e2 ]
    abs e = error $ "[] | Shouldn't use abs"
    signum e = error $ "[] | Shouldn't use signum"
    fromInteger i = Lit (Int (fromInteger i))


-----------------------------------
-- == Operations on syntax

declName :: Decl b -> Name
declName (Fun n _ _ _ _ _) = n

declName' :: Decl b -> String
declName' = nameToStr . declName
            
declParams :: Decl b -> [(b, Typ)]
declParams (Fun _ params _ _ _ _) = params
                                    
declAllocs :: Decl b -> [b]
declAllocs (Fun _ _ allocs _ _ _) = allocs

declsAllocs :: [Decl b] -> [b]
declsAllocs = concat . map declAllocs
                                    
declBody :: Decl b -> Stmt b
declBody (Fun _ _ _ body _ _) = body

declRet :: Decl b -> Maybe (Exp b)
declRet (Fun _ _ _ _ ret _) = ret
                                
seqStmt :: [Stmt b] -> Stmt b
seqStmt [] = Skip
seqStmt stmts = foldl (\acc s -> Seq acc s) (head stmts) (tail stmts)
                
isAtmExp :: Exp b -> Bool
isAtmExp (Var _) = True
isAtmExp (Lit _) = True
isAtmExp _ = False
             
isSimplExp :: Exp b -> Bool
isSimplExp (Var _) = True
isSimplExp (Lit _) = True
isSimplExp (Call _ es) = all isAtmExp es
isSimplExp (DistOp _ _ _ es) = all isAtmExp es
isSimplExp (Proj e es) = isAtmExp e && all isAtmExp es


-----------------------------------
-- == Canonicalizing statements
                         
type LStmt b = [LStmt' b]
data LStmt' b = LAtm (Stmt b)
              | LIf (Exp b) (LStmt b) (LStmt b)
              | LLoop LoopKind b (Gen b) (LStmt b)
              | LMapRed b b (Gen b) (LStmt b) (Exp b)

instance (Ord b) => Substitutable b (LStmt' b) (Exp b) where
    substP p x term = sub
        where
          sub (LAtm s) = LAtm (substP p x term s)
          sub (LIf e s1 s2) = LIf (substP p x term e) (map sub s1) (map sub s2)
          sub (LLoop lk y gen s)
              | x `p` y = LLoop lk y (substP p x term gen) s
              | otherwise = LLoop lk y (substP p x term gen) (map sub s)
          sub (LMapRed acc y gen s e)
              | x `p` y = LMapRed acc y (substP p x term gen) s (substP p x term e)
              | otherwise = LMapRed acc y (substP p x term gen) (map sub s) (substP p x term e)

                
splat :: (TypedVar b Typ) => Stmt b -> LStmt b
splat Skip = []
splat (Exp e) = [LAtm (Exp e)]
splat (Assign x e) = [LAtm (Assign x e)]
splat (Store x es uk e') = [LAtm (Store x es uk e')]
splat (Seq s1 s2) = splat s1 ++ splat s2
splat (If e s1 s2) = [LIf e (splat s1) (splat s2)]
splat (Loop lk x gen s) = [LLoop lk x gen (splat s)]
splat (MapRed acc x gen s e) = [LMapRed acc x gen (splat s) e]

unsplat :: (TypedVar b Typ) => LStmt b -> Stmt b
unsplat stmts = seqStmt (map unsplat' stmts)
                               
unsplat' :: (TypedVar b Typ) => LStmt' b -> Stmt b
unsplat' (LAtm s) = s
unsplat' (LIf e s1 s2) = If e (unsplat s1) (unsplat s2)
unsplat' (LLoop lk x gen s) = Loop lk x gen (unsplat s)
unsplat' (LMapRed acc x gen s e) = MapRed acc x gen (unsplat s) e

canonizeStmt :: (TypedVar b Typ) => Stmt b -> Stmt b
canonizeStmt = unsplat . splat

-----------------------------------
-- == Assigns

type Assign b = (b, Exp b)

assignToStmt :: Assign b -> Stmt b
assignToStmt (x, e) = Assign x e

assignsToStmt :: [Assign b] -> Stmt b
assignsToStmt = seqStmt . (map assignToStmt)
                      
assignsToStmt' :: [Assign b] -> Stmt b -> Stmt b
assignsToStmt' assigns s =
    case assigns of
      [] -> s
      _ -> Seq (assignsToStmt assigns) s


mkDensPt :: b -> [Exp b] -> Exp b
mkDensPt x [] = Var x
mkDensPt x idxs = Proj (Var x) idxs
