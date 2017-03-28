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


module CudaC.PllBlk where

import Data.Foldable (Foldable)
import Control.Monad.State
import Data.Traversable (Traversable)
import Control.Monad.Identity
import qualified Data.Map as Map
import Debug.Trace
    
import AstUtil.Var
import AstUtil.Pretty
import CudaC.BlkSyn
import Low.LowSyn hiding (splat, unsplat)


----------------------------------------------------------------------
-- = PllBlk Description
{-| [Note]

-}
    

-----------------------------------
-- == TODO: MOVEME


type SplitM b m = StateT (SplitSt b) m
data SplitSt b =
    SplitSt { sp_hist :: [Either (Stmt b) (LoopKind, b, Gen b)]
            , sp_split :: [(Maybe b, Stmt b, Exp b, Exp b)] }

    
pushHist :: (BasicVar b, Monad m) => Either (Stmt b) (LoopKind, b, Gen b) -> SplitM b m ()
pushHist h = modify (\st -> st { sp_hist = h : sp_hist st })

emitSplit :: (BasicVar b, Monad m) => Maybe b -> Stmt b -> Exp b -> Exp b -> SplitM b m ()
emitSplit x s acc ret = modify (\st -> st { sp_split = (x, s, acc, ret) : sp_split st })

             
reconHist :: (BasicVar b) => [Either (Stmt b) (LoopKind, b, Gen b)] -> Stmt b
reconHist (Left s : []) = s
reconHist (Left s : tl) = Seq s (reconHist tl)
reconHist (Right _ : []) = Skip
reconHist (Right (lk, x, gen) : tl) = Loop lk x gen (reconHist tl)
reconHist [] = Skip

               
splitStmt :: (BasicVar b, Monad m) => b -> Stmt b -> SplitM b m ()
splitStmt _ Skip = return ()
splitStmt _ (Exp e) = pushHist (Left (Exp e))    
splitStmt _ (Assign x e) = pushHist (Left (Assign x e))
splitStmt idx (Seq s1 s2) =
    do splitStmt idx s1
       splitStmt idx s2
splitStmt idx (Loop lk x gen s) =
    do hist <- gets sp_hist
       pushHist (Right (lk, x, gen))
       splitStmt idx s
       modify (\st -> st { sp_hist = hist })
-- TODO?? check that sk is the right kind??
splitStmt idx (Store x (Var i : es) sk e')          
    | not (isModLocal (idKind x)) && idx /= i =
        do -- pushHist (Left (Store (Var x) (Var i : es) sk e'))
           hist <- gets sp_hist
           emitSplit (Just i) (reconHist (reverse hist)) (Proj (Var x) (Var i : es)) e'
    | not (isModLocal (idKind x)) && idx == i =
        do hist <- gets sp_hist
           emitSplit Nothing (reconHist (reverse hist)) (Proj (Var x) (Var i : es)) e'
    | otherwise = pushHist (Left (Store x (Var i : es) sk e'))
splitStmt _ s =
    error $ "@splitStmt | Shouldn't happen: " ++ pprShow s

          
splitComp :: (Show b, BasicVar b, Monad m) => Comp b -> SplitM b m [(Maybe b, Comp b, Exp b, Exp b)]
-- splitComp (SingleHost s) = return [(Nothing, SingleHost s, 0, 0)]
-- splitComp (SingleDev s) = return [(Nothing, SingleDev s, 0, 0)]
splitComp (Single sc s) = return [(Nothing, Single sc s, 0, 0)]
splitComp (Block lk x gen s) =
    do splitStmt x s
       split <- gets sp_split
       return $ map (\(idx, s, acc, ret) -> (idx, Block lk x gen s, acc, ret)) split
splitComp c =
    error $ "@splitComp | Shouldn't happen: " ++ show c

          
runSplitComp :: (Show b, BasicVar b) => Comp b -> [(Maybe b, Comp b, Exp b, Exp b)]
runSplitComp c = 
    runIdentity ((runStateT (splitComp c) (SplitSt [] [])) >>= return . fst)



-----------------------------------
-- == TODO: MOVEME

interpGenExp :: (BasicVar b) => Map.Map b Int -> Exp b -> Maybe Int
interpGenExp env (Var x) = Map.lookup x env
interpGenExp _ (Lit (Int i)) = Just i
-- TODO
-- interpGenExp env (LibCall "sizeBVec" [ Var x ]) = Map.lookup x env
interpGenExp _ _ = Nothing

                   
interpGen :: (BasicVar b) => Map.Map b Int -> Gen b -> Maybe Int
interpGen env (Until e1 e2) =
    do i1 <- interpGenExp env e1
       i2 <- interpGenExp env e2
       return $ i2 - i1

              
compareGen :: (BasicVar b) => Map.Map b Int -> (LoopKind, b, Gen b) -> (LoopKind, b, Gen b) -> Ordering
compareGen env (_, _, gen1) (_, _, gen2) =
    case (interpGen env gen1, interpGen env gen2) of
      (Just i1, Just i2) -> compare i1 i2
      (Nothing, Just _) -> GT
      (Just _, Nothing) -> LT
      (Nothing, Nothing) -> EQ

                            
reassign :: (BasicVar b) => Exp b -> Exp b -> Stmt b
reassign ret = f
    where f (Var x) = Store x [] AtmInc ret
          f (Proj (Var x) es) = Store x es AtmInc ret
          f e = error $ "@reassign | Shouldn't happen: " ++ pprShow e

appendStmt :: (BasicVar b) => Stmt b -> Stmt b -> Stmt b
appendStmt s = append
    where
      append Skip = s
      append (Exp e) = Seq (Exp e) s
      append (Assign x e) = Seq (Assign x e) s
      append (Seq s1 s2) = Seq s1 (append s2)
      append (Loop lk x gen s') = Loop lk x gen (append s')
      -- append (IncAssign x e) = Seq (IncAssign x e) s
      append (Store e es sk e') = Seq (Store e es sk e') s

                                            
hoistComp :: (Show b, BasicVar b, Monad m) => Map.Map b Int -> (Maybe b, Comp b, Exp b, Exp b) -> m (Comp b)
-- hoistComp _ (_, SingleHost s, _, _) = return $ SingleHost s
-- hoistComp _ (_, SingleDev s, _, _) = return $ SingleDev s
hoistComp _ (_, Single sc s, _, _) = return $ Single sc s
hoistComp rtsizes (Just idx, Block lk x gen s, acc, ret) =
    case g s of
      Just (lk', x', gen') ->
          case (interpGen rtsizes gen, interpGen rtsizes gen') of
            (Just sz1, Just sz2) ->
                do traceM $ "ratioTest: " ++ show sz1 ++ " " ++ show sz2 ++ " " ++ show (ratioTest sz1 sz2)
                   if ratioTest sz1 sz2
                   then return $ Block lk x gen (appendStmt (reassign ret acc) s)
                   else return $ Reduce (Just (x', gen')) acc x gen (rest s) ret
            szs ->
                do traceM $ "Found nothing1 " ++ show szs
                   return $ Block lk x gen (appendStmt (reassign ret acc) s)          
      Nothing ->
          do traceM $ "Found nothing2 " ++ show (Block lk x gen s)
             return $ Block lk x gen (appendStmt (reassign ret acc) s)
    where
      ratioTest :: Int -> Int -> Bool
      ratioTest i1 i2 =
          let i1' = fromIntegral i1 :: Float
              i2' = fromIntegral i2 :: Float
          in
            i1' / i2' < 40
            
      g Skip = Nothing
      g (Exp _) = Nothing
      g (Assign _ _) = Nothing
      g (Seq s1 s2) =
          case g s1 of
            Just x -> Just x
            Nothing -> case g s2 of
                         Just x -> Just x
                         Nothing -> Nothing 
      g (Loop lk x gen s)
          | idx == x = Just (lk, x, gen)
          | otherwise = g s
      g (Store _ _ _ _) = Nothing
      
      rest Skip = Skip
      rest (Exp e) = Exp e
      rest (Assign x e) = Assign x e
      rest (Seq s1 s2) = Seq (rest s1) (rest s2)
      rest (Loop lk x gen s)
          | idx == x = s
          | otherwise = Loop lk x gen (rest s)
      rest (Store e es sk e') = Store e es sk e'       
hoistComp _ (Nothing, Block lk x gen s, Var y, e) =
    return $ Reduce Nothing (Var y) x gen s e
hoistComp _ (Nothing, Block lk x gen s, acc, ret) =
    return $ Block lk x gen (appendStmt (reassign ret acc) s)
hoistComp _ c = error $ "@hoistComp | Shouldn't happen: " ++ show c
