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

{-# LANGUAGE FlexibleContexts #-}

module Low.InlineLow 
    ( runInlineDecl ) where

import Control.Monad.Except
import Control.Monad.RWS

import AstUtil.Pretty
import AstUtil.Fresh
import AstUtil.Var
import Compile.CompData
import Low.LowSyn
import Low.LowpPrimSyn
import Core.CoreSyn
import Core.CoreTySyn
import Comm.DistSyn
import Comm.Prim
import qualified Low.LintLow as Lint
import Low.ProjLow

----------------------------------------------------------------------
-- = InlineLow Description
{-| [Note]

Input must:
1) only have atomic expressions
2) not have functional primitives

-}



-----------------------------------
-- == Types and operations

type InlineM b = RWST InlineRdr [()] [Stmt b] CompM
data InlineRdr = IR { ir_genSym :: GenSym
                    , ir_inLoop :: Bool }


withInLoop :: (TypedVar b Typ) => Bool -> InlineM b a -> InlineM b a
withInLoop inLoop =
    local (\rdr -> rdr { ir_inLoop = inLoop })
               
freshId :: (TypedVar b Typ) => Name -> IdKind -> Typ -> InlineM b b
freshId name ik ty =
    do genSym <- asks ir_genSym
       lift $ lift $ mkTyIdIO genSym name ik ty

emitStmt :: (TypedVar b Typ) => Stmt b -> InlineM b ()
emitStmt s = 
    modify (\st -> s : st)

-----------------------------------
-- == Transformation


tyOf :: (TypedVar b Typ) => Exp b -> Typ
tyOf (Var x) = getType' x
tyOf (Lit lit) = 
    case lit of
      Int _ -> IntTy
      Real _ -> RealTy
tyOf e = error $ "[Low.InlineLow] | Expected atomic expression but found: " ++ pprShow e


inlineDirichlet :: (TypedVar b Typ) => Dop -> DopMode -> [Exp b] -> InlineM b (Exp b)
inlineDirichlet dop dm es =
    case dop of
      Conj _ _ -> 
          do x <- freshId Anon Local IntTy
             y <- freshId Anon Local (tyOf (es !! 0))
             let gen = Until 0 (Call (PrimId DM_Mem PM_Fn SizeVec) [ es !! 1 ])
                 e1 = Proj (es !! 2) [ Var x ]
                 e2 = Proj (es !! 1) [ Var x ]
                 e_samp = DistOp Sample dm Gamma [ e1 + e2, Lit (Real 1.0) ]
                 s_ass = Assign y (es !! 0)
                 s_store = Store y [ Var x ] Update e_samp
                 s_samp = Loop Parallel x gen (seqStmt [ s_ass, s_store ])
                 e_norm = Call (PrimId DM_Mem PM_Fn NormVec) [ es !! 0 ]
                 s_norm = (Exp e_norm)
             emitStmt (seqStmt [ s_samp, s_norm ])
             return $ Lit (Int 0)
      _ -> return $ DistOp dop dm Dirichlet es


inlineDist :: (TypedVar b Typ) => Dop -> DopMode -> Dist -> [Exp b] -> InlineM b (Exp b)
inlineDist dop dm dist es =
    case dist of
      Dirichlet -> inlineDirichlet dop dm es
      _ -> return $ DistOp dop dm dist es


inlineDotProd :: (TypedVar b Typ) => PrimMode -> [Exp b] -> InlineM b (Exp b)
inlineDotProd pm es =
    case pm of
      PM_Fn -> 
          do idx <- freshId Anon Local IntTy
             acc <- freshId Anon Local RealTy
             let gen = Until 0 (Call (PrimId DM_Mem PM_Fn SizeVec) [ es !! 0 ])
                 e_vec1 = Proj (es !! 0) [ Var idx ]
                 e_vec2 = Proj (es !! 1) [ Var idx ]
                 s_ass = seqStmt [ Assign acc (Lit (Real 0))
                                 , Loop Sequential idx gen (Store acc [] Inc (e_vec1 * e_vec2)) ]
             emitStmt s_ass
             return $ Var acc
      PM_Grad 0 ->
          do idx <- freshId Anon Local IntTy
             v_dst <- freshId Anon Local (VecTy RealTy)
             let gen = Until 0 (Call (PrimId DM_Mem PM_Fn SizeVec) [ es !! 0 ])
                 e_adj = es !! 1
                 e_vec2 = Proj (es !! 3) [ Var idx ]
                 s_ass = seqStmt [ Assign v_dst (es !! 0)
                                 , Loop Parallel idx gen (Store v_dst [ Var idx ] Update (e_adj * e_vec2)) ]
             emitStmt s_ass
             return $ Lit (Int 0)
      PM_Grad 1 -> 
          do idx <- freshId Anon Local IntTy
             v_dst <- freshId Anon Local (VecTy RealTy)
             let gen = Until 0 (Call (PrimId DM_Mem PM_Fn SizeVec) [ es !! 0 ])
                 e_adj = es !! 1
                 e_vec1 = Proj (es !! 2) [ Var idx ]
                 s_ass = seqStmt [ Assign v_dst (es !! 0)
                                 , Loop Parallel idx gen (Store v_dst [ Var idx ] Update (e_adj * e_vec1)) ]
             emitStmt s_ass
             return $ Lit (Int 0)
      _ -> error $ "[Low.InlineLow] @inlineDotProd | Shouldn't happen"


inlinePrim :: (TypedVar b Typ) => DopMode -> PrimMode -> Prim -> [Exp b] -> InlineM b (Exp b)
inlinePrim dop pm prim es = 
    case prim of
      DotProd -> inlineDotProd pm es
      _ -> return $ Call (PrimId dop pm prim) es


inlineExp :: (TypedVar b Typ) => Exp b -> InlineM b (Exp b)
inlineExp (Var x) = return $ Var x
inlineExp (Lit lit) = return $ Lit lit
inlineExp (DistOp dop dm dist es) =
    do inLoop <- asks ir_inLoop
       if inLoop
       then inlineDist dop dm dist es
       else return $ DistOp dop dm dist es
inlineExp (Call ce es) = 
    case ce of
      FnId _ -> error $ "[Low.InlineLow] | TODO"
      PrimId dop pm prim ->
          do inLoop <- asks ir_inLoop
             if inLoop
             then inlinePrim dop pm prim es
             else return $ Call (PrimId dop pm prim) es
inlineExp (Proj e es) = return $ Proj e es


inlineStmt :: (TypedVar b Typ) => Stmt b -> InlineM b (Stmt b)
inlineStmt Skip = return Skip
inlineStmt (Exp e) = 
    do saved <- get
       e' <- inlineExp e
       s_inline <- get >>= return . seqStmt
       put saved
       let s = case e' of
                 Lit (Int 0) -> Skip
                 _ -> Exp e'
       return $ seqStmt [ s_inline, s ]
inlineStmt (Assign x e) = 
    do saved <- get
       e' <- inlineExp e
       s_inline <- get >>= return . seqStmt
       put saved
       return $ seqStmt [ s_inline, Assign x e' ]
inlineStmt (Store x es uk e) = 
    do saved <- get
       e' <- inlineExp e
       s_inline <- get >>= return . seqStmt
       put saved
       return $ seqStmt [ s_inline, Store x es uk e' ]
inlineStmt (Seq s1 s2) = 
    do s1' <- inlineStmt s1
       s2' <- inlineStmt s2
       return $ Seq s1' s2'
inlineStmt (If e s1 s2) = 
    do s1' <- inlineStmt s1
       s2' <- inlineStmt s2
       return $ If e s1' s2'
inlineStmt (Loop lk x gen s) = 
    do s' <- withInLoop True (inlineStmt s)
       return $ Loop lk x gen s'
inlineStmt (MapRed acc x gen s e) = 
    do s' <- withInLoop True (inlineStmt s)
       return $ MapRed acc x gen s' e


inlineDecl :: (TypedVar b Typ) => Decl b -> InlineM b (Decl b)
inlineDecl (Fun name params allocs body retExp retTy) = 
    do body' <- inlineStmt body
       return $ Fun name params allocs body' retExp retTy


-----------------------------------
-- == Top-level

{-
runInlineStmt :: (TypedVar b Typ) => CompInfo -> Stmt b -> CompM (Stmt b)
runInlineStmt cinfo s =
    do (s', _, _) <- runRWST (inlineStmt s) (IR (getGenSym cinfo)) []
       s'' <- runProjStmt cinfo s'
       return s''
-}

runInlineDecl :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> Decl b -> CompM (Decl b)
runInlineDecl cinfo copt inferCtx decl =
    do let genSym = getGenSym cinfo
       (decl', _, _) <- runRWST (inlineDecl decl) (IR genSym False) []
       decl'' <- runProjDecl cinfo copt inferCtx decl'
       runLint copt decl'' (Lint.runLintDecl cinfo False inferCtx)
