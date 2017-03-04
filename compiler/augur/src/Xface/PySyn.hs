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

module Xface.PySyn where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Text.PrettyPrint
    
import AstUtil.Pretty
import AstUtil.Var

    
----------------------------------------------------------------------
-- = PySyn Description
{-| [Note]

Contains Python syntax.

-}


----------------------------------------------------------------------
-- = Syntax

data Decl b = Fun Name [b] (Stmt b)
            deriving (Show, Functor, Foldable, Traversable)

data Stmt b = Exp (Exp b)
            | Assign b (Exp b)
            | Store b [Exp b] (Exp b)
            | Seq (Stmt b) (Stmt b)
            -- | If (Exp b) (Stmt b) (Stmt b)
            | Loop b (Gen b) (Stmt b)
            | Return (Exp b)
              deriving (Show, Functor, Foldable, Traversable)

data Gen b = Until (Exp b) (Exp b)
           deriving (Show, Functor, Foldable, Traversable)
              
data Exp b = Var b
           | Lit Lit
           | Gen (Gen b)
           | Call String [Exp b]
           | Binop (Exp b) Bop (Exp b)
           | Tup [Exp b]
           | List [Exp b]
           | Proj (Exp b) [Exp b]
             deriving (Show, Functor, Foldable, Traversable)

data Lit = Int Int
         | Dbl Double
         | PyStr String
           deriving (Show)

data Bop = Plus
         | Minus
         | Times
         | Div
         | EqEq
         | LAnd
         deriving (Show)

                    
----------------------------------------------------------------------
-- = Instances

instance (Pretty b) => Pretty (Decl b) where
    ppr (Fun name params body) =
        hang (ppr "def" <+> ppr name <> parens (sepBy commasp params) <> colon) 2 (ppr body)
        
instance (Pretty b) => Pretty (Stmt b) where
    ppr (Exp e) = ppr e    
    ppr (Assign x e) = ppr x <+> text "=" <+> ppr e
    ppr (Store x es e) =
        ppr x <> brackets (sepBy commasp es) <+> text "=" <+> ppr e
    ppr (Seq s1 s2) = vcat [ ppr s1, ppr s2 ]
    ppr (Loop x gen s) =
        vcat [ hang (text "for" <+> ppr x <+> text " in " <+> ppr gen <> colon) 2 (ppr s) ]
    ppr (Return e) =
        text "return" <+> ppr e

instance (Pretty b) => Pretty (Gen b) where
    ppr (Until e1 e2) = text "range" <> parens (ppr e1 <> comma <+> ppr e2)

instance (Pretty b) => Pretty (Exp b) where
    ppr (Var x) = ppr x
    ppr (Lit lit) = ppr lit
    ppr (Gen gen) = ppr gen
    ppr (Call fn es) = text fn <> parens (sepBy commasp es)
    ppr (Binop e1 bop e2) = ppr e1 <+> ppr bop <+> ppr e2
    ppr (Tup es) = parens (sepBy commasp es)
    ppr (List es) = brackets (sepBy commasp es)
    ppr (Proj e es) = ppr e <> brackets (sepBy commasp es)

instance Pretty Lit where
    ppr (Int i) = int i
    ppr (Dbl d) = double d
    ppr (PyStr str) = text "\'" <> text str <> text "\'"

instance Pretty Bop where
    ppr Plus = text "+"
    ppr Minus = text "-"
    ppr Times = text "*"
    ppr Div = text "/"
    ppr EqEq = text "=="
    ppr LAnd = text "&&"
                  
seqStmt :: [Stmt b] -> Stmt b
seqStmt [] = Exp (Lit (Int 0))
seqStmt stmts = foldl (\acc s -> Seq acc s) (head stmts) (tail stmts)
