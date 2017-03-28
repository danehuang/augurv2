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

module CudaC.SplitBlk where

import Control.Monad.State
import Control.Monad.RWS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
    
import AstUtil.Var
import AstUtil.VarOp
import AstUtil.Pretty
import CudaC.BlkSyn
import CudaC.RtSize
import Compile.CompData
import Core.CoreTySyn
import Low.DepLow
import Low.LowSyn
   

----------------------------------------------------------------------
-- = SplitBlk Description
{-| [Note]

-}



-----------------------------------
-- == Types and operations

type LoopM b = RWST (LoopRdr b) [()] (LoopSt b) CompM
data LoopRdr b =
    LR { lr_split :: Bool }
data LoopSt b =
    LS { ls_hist :: [Stmt b]
       , ls_split :: [Stmt b]
       , ls_localCtx :: Map.Map b (Exp b) }

    
withSplit :: (TypedVar b Typ) => Bool -> LoopM b a -> LoopM b a
withSplit split =
    local (\rdr -> rdr { lr_split = split })

          
clearHist :: (TypedVar b Typ) => LoopM b ()
clearHist =
    modify (\st -> st { ls_hist = [] })
    
currHist :: (TypedVar b Typ) => LoopM b [Stmt b]
currHist = gets ls_hist
              
emitHist :: (TypedVar b Typ) => Stmt b -> LoopM b ()
emitHist s =
    modify (\st -> st { ls_hist = s : ls_hist st })


saveCtx :: (TypedVar b Typ) => LoopM b ([Stmt b], Map.Map b (Exp b))
saveCtx =
    do hist <- currHist
       localCtx <- gets ls_localCtx
       clearHist
       return (hist, localCtx)

restoreCtx :: (TypedVar b Typ) => [Stmt b] -> Map.Map b (Exp b) -> LoopM b ()
restoreCtx hist localCtx =
    modify (\st -> st { ls_hist = hist
                      , ls_localCtx = localCtx })
              
           
emitLocal :: (TypedVar b Typ) => b -> Exp b -> LoopM b ()
emitLocal x e =
    modify (\st -> st { ls_localCtx = Map.insert x e (ls_localCtx st) })
           
emitSplit :: (TypedVar b Typ) => Stmt b -> LoopM b ()
emitSplit s =
    modify (\st -> st { ls_split = s : ls_split st })

           
-----------------------------------
-- == Splitting

collectLocal :: (TypedVar b Typ) => [b] -> Map.Map b (Exp b) -> Stmt b
collectLocal vs localCtx =
    seqStmt $ map (\v -> if isLocalId v
                         then case Map.lookup v localCtx of
                                Just e -> Assign v e
                                Nothing -> Skip -- TODO: HACK manage contexts
                         else Skip
                  ) vs


splitLoop :: (TypedVar b Typ) => Stmt b -> LoopM b ()
splitLoop Skip =
    return ()
splitLoop (Exp e) =
    emitHist (Exp e)
splitLoop (Assign x e) =
    do emitHist (Assign x e)
       if isLocalId x
       then emitLocal x e
       else return ()
splitLoop (Store x es uk e) =
    emitHist (Store x es uk e)
splitLoop (Seq s1 s2) =
    do splitLoop s1
       splitLoop s2       
splitLoop (If e s1 s2) =
    do (hist, localCtx) <- saveCtx
       splitLoop s1
       (stmts1, _) <- saveCtx
       splitLoop s2
       stmts2 <- currHist
       restoreCtx hist localCtx
       emitHist (If e (seqStmt (reverse stmts1)) (seqStmt (reverse stmts2)))
splitLoop (Loop lk x gen s) =
    do split <- asks lr_split
       if split          
       then do (hist, localCtx) <- saveCtx
               withSplit False (splitLoop s)
               stmts <- currHist
               let s' = Loop lk x gen (seqStmt (reverse stmts))
                   (genSet, killSet) = gkStmt s'
                   deps = Set.toList (Set.delete x (genSet `Set.difference` killSet))
                   s'' = seqStmt [ collectLocal deps localCtx, s' ]
               emitSplit (seqStmt (reverse (s'' : hist)))
               clearHist
       else do (hist, localCtx) <- saveCtx
               splitLoop s
               stmts <- currHist
               restoreCtx hist localCtx
               emitHist (Loop lk x gen (seqStmt (reverse stmts)))           
splitLoop (MapRed acc x gen s e) =
    do (hist, localCtx) <- saveCtx
       splitLoop s
       stmts <- currHist
       restoreCtx hist localCtx
       emitHist (MapRed acc x gen (seqStmt (reverse stmts)) e)


runSplitLoop :: (TypedVar b Typ) => Stmt b -> CompM [Stmt b]
runSplitLoop s =
    do (_, LS _ split _, _) <- runRWST (splitLoop s) (LR True) (LS [] [] Map.empty)
       return (reverse split)

                
-----------------------------------
-- == Commuting

              
colHdNrmStmt :: (TypedVar b Typ) => Stmt b -> Maybe (Stmt b)
colHdNrmStmt s =
    let stmts = splat s
        foo = foldl (\acc s' ->
                         case s' of
                           LAtm Skip -> acc
                           LAtm (Assign x e) ->
                               case acc of
                                 Just acc' -> Just (subst x e acc')
                                 Nothing -> Nothing
                           _ -> Nothing
                    ) (Just (last stmts)) (init stmts)
    in
      case foo of
        Just s' -> Just (unsplat [s'])
        _ -> Nothing


tryCommComp :: (TypedVar b Typ) => Map.Map b Int -> Comp b -> CompM (Bool, Comp b)
tryCommComp rtSizeCtx comp =
    go comp
    where
      go (Single sc s) = return (False, Single sc s)
      go (Block lk x gen s) =
          case colHdNrmStmt s of
            Just (Loop lk' x' gen' s') ->
                case compareGen rtSizeCtx gen gen' of
                  LT -> return (True, Block lk' x' gen' (Loop lk x gen s'))
                  _ -> return (False, Block lk x gen s)
            _ -> return (False, Block lk x gen s)          
      go (Reduce ctx acc x gen s e) = return (False, Reduce ctx acc x gen s e)



-----------------------------------
-- == Top-level
              
runSplitComp :: (TypedVar b Typ) => Map.Map b Int -> Comp b -> CompM [Comp b]
runSplitComp rtSizeCtx comp =
    do debugM "[CudaC.SplitBlk]" $ "@runSplitComp | Input: " ++ pprShow comp
       comps <- splitComp comp
       (chgs, comps') <- mapM (tryCommComp rtSizeCtx) comps >>= return . unzip
       debugM "[CudaC.SplitBlk]" $ "@runSplitComp | Changes " ++ show chgs ++ " with split\n" ++ pprShowLs comps'
       if any (\x -> x) chgs
       then return comps'
       else return [ comp ]
    where
      splitComp (Single sc s) =
          return [ Single sc s ]
      splitComp (Block lk x gen s) =
          do split <- runSplitLoop s
             mapM (\s' -> return $ Block lk x gen s') split
      splitComp (Reduce ctx acc x gen s e) =
          return [ Reduce ctx acc x gen s e ]

        
