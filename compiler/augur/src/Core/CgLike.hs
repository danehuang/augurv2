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

module Core.CgLike
    ( runLLFn
    , runLLFnOne) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.RWS
import qualified Data.Set as Set
import Debug.Trace
    
import AstUtil.Var
import AstUtil.Fresh
import AstUtil.Pretty
import Core.DensSyn
import Comm.DistSyn
import Comm.Prim
import qualified Low.LowSyn as L
import Core.CoreSyn
import Core.CoreTySyn
import qualified Low.LintLow as Lint
import Compile.CompData
import Core.CgCore
import Core.RwCore as RW



----------------------------------------------------------------------
-- = CgLike Description
{-| [Note]

Generate log-likelihood function from density function.

Dens (TVar Typ)        (simpl expr, types)
       |
Low P.Prim (TVar Typ)  (simpl expr, types)

-}


    
-----------------------------------
-- == Types and operations
    
type LLM b = ReaderT (LLRdr b) CompM

data LLRdr b =
    LLRdr { cgr_vll :: b
          , cgr_promote :: Bool
          , llr_GenSym :: GenSym }

freshId :: (TypedVar b Typ) => Typ -> LLM b b
freshId ty =
    do genSym <- asks llr_GenSym
       lift $ lift $ mkTyIdIO genSym Anon Local ty

    
freshLLId :: (TypedVar b Typ) => LLM b b
freshLLId = freshId RealTy

            
withLLVar :: (TypedVar b Typ) => b -> LLM b a -> LLM b a
withLLVar v = local (\rdr -> rdr { cgr_vll = v })


withPromote :: Bool -> LLM b a -> LLM b a
withPromote promote = local (\rdr -> rdr { cgr_promote = promote })

                      
-----------------------------------
-- == Transformation

llFnBody' :: (TypedVar b Typ) => Fn b -> LLM b (L.Stmt b)
llFnBody' (Dens Dirac pt es) =
    do let e' = cgExp (es !! 0)
           idxs' = map cgExp (densPtIdx' pt)
       return $ L.Store (densPtVar pt) idxs' L.Update e'
llFnBody' (Dens dist pt es) =
    do promote <- asks cgr_promote
       if promote
       then
           do x <- freshId IntTy
              v_ll <- asks cgr_vll
              v_ll' <- freshLLId
              v_res <- freshLLId
              let gen' = L.Until 0 1
                  es' = cgExp pt : map cgExp es
                  s = L.Store v_ll' [] L.Inc (L.DistOp LL DM_Fn dist es')
              let s' = L.seqStmt [ L.Assign v_ll' (L.Lit (L.Real 0)), s ]
              return $ L.seqStmt [ L.MapRed v_res x gen' s' (L.Var v_ll')
                                 , L.Store v_ll [] L.Inc (L.Var v_res) ]
       else
           do v_ll <- asks cgr_vll
              let es' = cgExp pt : map cgExp es
              return $ L.Store v_ll [] L.Inc (L.DistOp LL DM_Fn dist es')
llFnBody' (Ind fn conds) =
    do let conds' = map cgIndCond conds
       s <- llFnBody' fn
       return $ L.If (andExp conds') s L.Skip
llFnBody' (Let x e fn) =
    do s <- llFnBody' fn
       return $ L.Seq (L.Assign x (cgExp e)) s
llFnBody' (Prod fn1 fn2) =
    do s1 <- case fn1 of
               Dens _ _ _ -> withPromote True (llFnBody' fn1)
               _ -> llFnBody' fn1
       s2 <- case fn2 of
               Dens _ _ _ -> withPromote True (llFnBody' fn2)
               _ -> llFnBody' fn2
       return $ L.Seq s1 s2
llFnBody' (Pi x gen fn) =
    do let gen' = cgGen gen
       v_ll <- asks cgr_vll
       v_ll' <- freshLLId
       v_res <- freshLLId
       s <- withLLVar v_ll' (llFnBody' fn)
       let s' = L.seqStmt [ L.Assign v_ll' (L.Lit (L.Real 0)), s ]
       return $ L.seqStmt [ L.MapRed v_res x gen' s' (L.Var v_ll')
                          , L.Store v_ll [] L.Inc (L.Var v_res) ]

       
llFnDecl :: (TypedVar b Typ) => Name -> Fn b -> LLM b (L.Decl b)
llFnDecl name fn =
    do vll <- asks cgr_vll
       body <- llFnBody' fn       
       let params = []
           allocs = []
           body' = L.Seq (L.Assign vll (L.Lit (L.Real 0))) body
           retExp = Just (L.Var vll)
       return $ L.Fun name params allocs body' retExp RealTy


             
-----------------------------------
-- == Types and operations 2
               
type LLM' b = RWST (LLRdr' b) [()] (LLSt' b) CompM

data LLRdr' b =
    LLRdr' { cgr_vll' :: b
           , cgr_genSym :: GenSym
           }

data LLSt' b =
    LS' { ls_seen :: Set.Set b }


freshLLId' :: (TypedVar b Typ) => LLM' b b
freshLLId' =
    do genSym <- asks cgr_genSym
       lift $ lift $ mkTyIdIO genSym Anon Local RealTy

            
withLLVar' :: (TypedVar b Typ) => b -> LLM' b a -> LLM' b a
withLLVar' v = local (\rdr -> rdr { cgr_vll' = v })

               
-----------------------------------
-- == Transformations 2
                           
llFnBodyTl :: (TypedVar b Typ) => Fn b -> LLM' b (L.Stmt b)
llFnBodyTl (Dens Dirac pt es) =
    do let e' = cgExp (es !! 0)
           idxs' = map cgExp (densPtIdx' pt)
       return $ L.Store (densPtVar pt) idxs' L.Update e'
llFnBodyTl (Dens dist pt es) =
    do vll <- asks cgr_vll'
       let es' = cgExp pt : map cgExp es
       return $ L.Store vll [] L.Inc (L.DistOp LL DM_Fn dist es')
llFnBodyTl (Ind fn conds) =
    do let conds' = map cgIndCond conds
       s <- llFnBodyTl fn
       return $ L.If (andExp conds') s L.Skip
llFnBodyTl (Let x e fn) =
    do s <- llFnBodyTl fn
       return $ L.Seq (L.Assign x (cgExp e)) s
llFnBodyTl (Prod fn1 fn2) =
    do s1 <- llFnBodyTl fn1
       s2 <- llFnBodyTl fn2
       return $ L.Seq s1 s2
llFnBodyTl (Pi x gen fn) =
    do seen <- gets ls_seen
       if Set.member x seen
       then llFnBodyTl fn
       else
           do let gen' = cgGen gen
              v_ll <- asks cgr_vll'
              v_ll' <- freshLLId'
              v_res <- freshLLId'
              s <- withLLVar' v_ll' (llFnBodyTl fn)
              let s' = L.seqStmt [ L.Assign v_ll' (L.Lit (L.Real 0)), s ]
              return $ L.seqStmt [ L.MapRed v_res x gen' s' (L.Var v_ll')
                                 , L.Store v_ll [] L.Inc (L.Var v_res) ]
            

llFnBodyHd :: (TypedVar b Typ) => Fn b -> LLM' b (L.Stmt b)
llFnBodyHd (Dens Dirac pt es) =
    do let e' = cgExp (es !! 0)
           idxs' = map cgExp (densPtIdx' pt)
       return $ L.Store (densPtVar pt) idxs' L.Update e'
llFnBodyHd (Dens dist pt es) =
    do vll <- asks cgr_vll'
       let es' = cgExp pt : map cgExp es
       return $ L.Store vll [] L.Inc (L.DistOp LL DM_Fn dist es')
llFnBodyHd (Ind fn conds) =
    do let conds' = map cgIndCond conds
       s <- llFnBodyHd fn
       return $ L.If (andExp conds') s L.Skip
llFnBodyHd (Let x e fn) =
    do s <- llFnBodyHd fn
       return $ L.Seq (L.Assign x (cgExp e)) s
llFnBodyHd (Prod _ _) = error $ "[CgLike] @llFnBodyHd | Shouldn't happen"
llFnBodyHd (Pi x _ fn) =
    do s <- llFnBodyHd fn
       modify (\st -> st { ls_seen = Set.insert x (ls_seen st) })
       return $ s  

              
llFnDeclOne :: (TypedVar b Typ) => b -> Fn b -> LLM' b ([b], L.Decl b)
llFnDeclOne v_mod fn =
    do vll <- asks cgr_vll'
       let (hdFn, tlFns) = RW.split v_mod fn
       s_hd <- llFnBodyHd hdFn
       s_tl <- llFnBodyTl (prodFn tlFns)
       let name = mkCompName "ll" (varName v_mod)
           idxs = densPtIdx (gatherDensPt hdFn)
           params = map (\idx -> (setIdKind idx GridIdx, IntTy)) idxs
           allocs = []           
           body' = L.seqStmt $ L.Assign vll (L.Lit (L.Real 0)) : [ s_hd, s_tl ]
           retExp = Just (L.Var vll)
       return (idxs, L.Fun name params allocs body' retExp RealTy)     


-----------------------------------
-- == Top-level

runLLFn :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> Name -> Fn b -> CompM (L.Decl b)
runLLFn cinfo copt inferCtx name fn =
    do vll <- lift $ mkIdIO (getGenSym cinfo) Anon Local
       v <- runReaderT (llFnDecl name fn) (LLRdr vll False (getGenSym cinfo))
       runLint copt v (Lint.runLintDecl cinfo False inferCtx)

              
runLLFnOne :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> b -> Fn b -> CompM ([b], L.Decl b)
runLLFnOne cinfo copt inferCtx v_mod fn =
    do let genSym = getGenSym cinfo
       vll <- lift $ mkTyIdIO genSym Anon Local RealTy
       ((projIdx, v), _, _) <- runRWST (llFnDeclOne v_mod fn) (LLRdr' vll genSym) (LS' Set.empty)
       v' <- runLint copt v (Lint.runLintDecl cinfo False inferCtx)
       return (projIdx, v')
