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

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts #-}

module CudaC.BlkSyn where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.Set as Set
import Text.PrettyPrint
import Debug.Trace

    
import AstUtil.Var
import AstUtil.VarOp
import AstUtil.Pretty
-- import Low.LowSyn hiding (splat, unsplat)
import Low.LowSyn
import Core.CoreTySyn


----------------------------------------------------------------------
-- = BlkSyn Description
{-| [Note]

-}


-----------------------------------
-- == Syntax

{-| [Note]

for (x <- gen_x) {
  acc = mapred(y <- gen_y) {
    s ;
    ret e;
  }
}

-}

data Comp b = Single SingleComp (Stmt b)
            | Block LoopKind b (Gen b) (Stmt b)
            | Reduce (Maybe (b, (Gen b))) (Exp b) b (Gen b) (Stmt b) (Exp b)
              deriving (Show, Functor, Foldable, Traversable)

data SingleComp = HostComp
                | KernelComp
                  deriving (Eq, Show)

                       
-----------------------------------
-- == Instances

instance (Pretty b) => Pretty (Comp b) where
    ppr (Single sc s) =
        vcat [ hang (ppr sc <+> text "Single" <+> lbrace) 2 (ppr s), rbrace ]
    ppr (Block lk x gen s) =
        vcat [ hang (text "Block" <> parens (ppr lk <> comma <+> ppr x <+> text "<-" <+> ppr gen) <+> lbrace) 2 (ppr s), rbrace ]
    ppr (Reduce ctx acc x gen s retExp) =
        vcat [ hang (ppr acc <+> text "=" <+> text "MapRed" <> parens (ppr x <+> text "<-" <+> ppr gen) <+> lbrace) 2 body, rbrace ]
        where
          body = vcat [ ppr s, text "return" <+> ppr retExp]

instance Pretty SingleComp where
    ppr HostComp = text "host"
    ppr KernelComp = text "kernel"
                 
-----------------------------------
-- == Operations on syntax
    

isLocalId :: (TypedVar b Typ) => b -> Bool
isLocalId = isModLocal . idKind

isLocalExp :: (TypedVar b Typ) => Exp b -> Bool
isLocalExp e = all isLocalId (Set.toList (fvs e))

isLocalGen :: (TypedVar b Typ) => Gen b -> Bool
isLocalGen (Until e1 e2) = isLocalExp e1 && isLocalExp e2
               
isLocalStmt :: (TypedVar b Typ) => Stmt b -> Bool
isLocalStmt Skip = True
isLocalStmt (Exp e) = isLocalExp e
isLocalStmt (Assign x e) = isLocalId x && isLocalExp e
isLocalStmt (Store x es _ e) = isLocalId x && all isLocalExp es && isLocalExp e
isLocalStmt (Seq s1 s2) = isLocalStmt s1 && isLocalStmt s2
isLocalStmt (If e s1 s2) = isLocalExp e && isLocalStmt s1 && isLocalStmt s2
isLocalStmt (Loop _ x gen s) = isLocalId x && isLocalGen gen && isLocalStmt s
isLocalStmt (MapRed acc x gen s e) = isLocalId acc && isLocalId x && isLocalGen gen && isLocalStmt s && isLocalExp e


canonizeBlk :: (TypedVar b Typ) => Comp b -> Comp b
canonizeBlk (Single sc s) = Single sc (canonizeStmt s)
canonizeBlk (Block lk x gen s) = Block lk x gen (canonizeStmt s)
canonizeBlk (Reduce ctx acc x gen s ret) = Reduce ctx acc x gen (canonizeStmt s) ret
                                     
partitionStmt :: (TypedVar b Typ) => Stmt b -> [Comp b]
partitionStmt s = map canonizeBlk (go (break isLoop (splat s)))
    where 
      go ([], []) =
          []
      go (left, []) =
          [ mkSingle (unsplat left) ]
      go ([], LLoop lk x gen s' : rest) =
          mkBlock (LLoop lk x gen s') : go (break isLoop rest)
      go ([], LMapRed acc x gen s' ret : rest) =
          Reduce Nothing (Var acc) x gen (unsplat s') ret : go (break isLoop rest)
      go (left, LLoop lk x gen s' : rest) =
          mkSingle (unsplat left) : mkBlock (LLoop lk x gen s') : go (break isLoop rest)
      go (left, LMapRed acc x gen s' ret : rest) =
          mkSingle (unsplat left) : Reduce Nothing (Var acc) x gen (unsplat s') ret : go (break isLoop rest)      
      go (left, right) =
          error $ "[BlkSyn] @partition | Shouldn't happen: " ++ pprShow (unsplat left) ++ "vs\n\n" ++ pprShow (unsplat right)

      mkSingle s'
          | isLocalStmt s' = Single HostComp s'
          | otherwise = Single KernelComp s'
                
      mkBlock (LLoop Sequential x gen s') =
          mkSingle (unsplat [LLoop Sequential x gen s'])
      mkBlock (LLoop lk x gen s') =
          Block lk x gen (unsplat s')
      mkBlock _ = error "[BlkSyn] @block | Shouldn't happen"
                 
      isLoop (LLoop _ _ _ _) = True
      isLoop (LMapRed _ _ _ _ _) = True
      isLoop _ = False






                 

{-
data AStmt b = AExp (Exp b)
             | AAssign b (Exp b)
             | AIncAssign b (Exp b)
             | ALoop LoopKind b (Gen b) (CStmt b)
             | AIf (Exp b) (CStmt b) (CStmt b)
             | AStore b [Exp b] UpKind (Exp b)
             | AMapRed b b (Gen b) (CStmt b) (Exp b)
               deriving (Show, Functor, Foldable, Traversable)
                        
type CStmt b = [AStmt b]
-}
                 
{-          
splat :: (BasicVar b) => Stmt b -> CStmt b
splat Skip = []
splat (Exp e) = [AExp e]
splat (Assign x e) = [AAssign x e]
splat (Seq s1 s2) = splat s1 ++ splat s2
splat (Loop lk x gen s) = [ALoop lk x gen (splat s)]
splat (If e s1 s2) = [AIf e (splat s1) (splat s2)]
splat (Store e es sk e') = [AStore e es sk e']
splat (MapRed acc x gen s e) = [AMapRed acc x gen (splat s) e]

                               
unsplat :: (BasicVar b) => CStmt b -> Stmt b
unsplat [] = Skip
unsplat (s:rest) =
    case rest of
      [] -> go s
      _ -> Seq (go s) (unsplat rest)
    where
      go (AExp e) = Exp e
      go (AAssign x e) = Assign x e
      go (AIncAssign x e) = Assign x e
      go (ALoop lk x gen s') = Loop lk x gen (unsplat s')
      go (AIf e s1 s2) = If e (unsplat s1) (unsplat s2)
      go (AStore e es sk e') = Store e es sk e'
      go (AMapRed acc x gen s' e) = MapRed acc x gen (unsplat s') e

                          
partitionStmt :: (BasicVar b) => Stmt b -> [Comp b]
partitionStmt s = go (break isLoop (splat s) )
    where
      go ([], []) =
          []
      go (left, []) =
          [Single (unsplat left)]
      go ([], ALoop lk x gen s' : rest) =
          block (ALoop lk x gen s') : go (break isLoop rest)
      go (left, ALoop lk x gen s' : rest) =
          Single (unsplat left) : block (ALoop lk x gen s') : go (break isLoop rest)
      go (left, right) =
          error $ "@parition | Shouldn't happen: " ++ pprShow (unsplat left) ++ "vs\n\n" ++ pprShow (unsplat right)

      block (ALoop Sequential x gen s') =
          Single (unsplat [ALoop Sequential x gen s'])
      block (ALoop lk x gen s') =
          Block lk x gen (unsplat s')
      block _ = error "@block | Shouldn't happen"
                 
      isLoop (ALoop _ _ _ _) = True
      isLoop _ = False
-}
