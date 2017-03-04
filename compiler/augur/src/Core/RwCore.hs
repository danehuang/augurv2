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

module Core.RwCore where

import Control.Monad.RWS
import Control.Monad.Identity
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Text.PrettyPrint
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Var
import AstUtil.VarOp
import AstUtil.AlphaEquiv
import Core.DensSyn
import Comm.DistSyn
import Core.CoreTySyn
    

----------------------------------------------------------------------
-- = RwCore Description
{-| [Note]

Transformations on Density functions.

-}


-----------------------------------
-- == Partition function

split :: (BasicVar b) => b -> Fn b -> (Fn b, [Fn b])
split v fn =
    case List.partition (containsPt v) (unfactor fn) of
      ([fn'], fns') -> (fn', fns')
      (fns1, fns2) -> error $ "[RwCore] @split | Shouldn't happen: " ++ rendSepBy commasp fns1 ++ " and " ++ rendSepBy commasp fns2


fullCond :: (BasicVar b) => b -> Fn b -> Fn b
fullCond v fn =
    prodFn (filter (\fn' -> Set.member v (fvs fn')) (unfactor fn))


fullConds :: (BasicVar b) => [b] -> Fn b -> [Fn b]
fullConds vs fn = map (\v -> fullCond v fn) vs
           
fullConds' :: (BasicVar b) => [b] -> Fn b -> Fn b
fullConds' vs fn = prodFn (fullConds vs fn)


                  
                  
-----------------------------------
-- == Normalizing products
             
type LFn b = [LFn' b]
data LFn' b = LDens Dist (Exp b) [Exp b]
            | LInd (LFn b) [IndCond b]
            | LLet b (Exp b) (LFn b)
            | LPi b (Gen b) (LFn b)

splat :: (BasicVar b) => Fn b -> LFn b
splat (Dens dist pt es) = [LDens dist pt es]
splat (Ind fn conds) = [LInd (splat fn) conds]
splat (Let x e fn) = [LLet x e (splat fn)]
splat (Prod fn1 fn2) = splat fn1 ++ splat fn2
splat (Pi x gen fn) = [LPi x gen (splat fn)]

unsplat' :: (BasicVar b) => LFn' b -> Fn b
unsplat' (LDens dist pt es) = Dens dist pt es
unsplat' (LInd fn cond) = Ind (unsplat fn) cond
unsplat' (LLet x e fn) = Let x e (unsplat fn)
unsplat' (LPi x gen fn) = Pi x gen (unsplat fn)

unsplat :: (BasicVar b) => LFn b -> Fn b
unsplat = prodFn . map unsplat'


          
-----------------------------------
-- == Unfactor

unfactorK' :: (BasicVar b) => Fn b -> ([Fn b] -> [Fn b]) -> [Fn b]
unfactorK' (Dens dist ept es) k =    
    k [Dens dist ept es]
unfactorK' (Ind fn cond) k =
    unfactorK' fn (\fns' -> k (map (\fn' -> Ind fn' cond) fns'))
unfactorK' (Let x e fn) k =
    unfactorK' fn (\fns' -> k (map (\fn' -> Let x e fn') fns'))
unfactorK' (Prod fn1 fn2) k =
    unfactorK' fn1 (\fns1' -> unfactorK' fn2 (\fns2' -> k (fns1' ++ fns2')))
unfactorK' (Pi x gen fn) k =
    unfactorK' fn (\fns' -> k (map (\fn' -> Pi x gen fn') fns'))

               
unfactor :: (BasicVar b) => Fn b -> [Fn b]
unfactor fn = unfactorK' fn (\x -> x)


unfactorInOrd :: (BasicVar b) => [b] -> Fn b -> [Fn b]
unfactorInOrd ord fn =
    let ctx = Map.fromList (map (\fn' -> (densPtVar (gatherDensPt fn'), fn')) (unfactor fn))
    in
      map fromJust (filter isJust (map (\v -> Map.lookup v ctx) ord))
              
              
-----------------------------------
-- == Factor
              
genEquivClass' :: (BasicVar b) => [(b, Gen b)] -> [[(b, Gen b)]]
genEquivClass' [] = []
genEquivClass' (gen:[]) = [[gen]]
genEquivClass' (gen:gens) =
    let (equiv, notEquiv) = List.partition (\gen' -> snd gen =\= snd gen') gens
        -- remove duplicates
        equivCls = map unGen (List.nub (map Gen' (gen : equiv)))
    in
      equivCls : genEquivClass' notEquiv

               
genEquivClass :: (BasicVar b) => [(b, Gen b)] -> [[(b, Gen b)]]
genEquivClass gens =
    let equiv = genEquivClass' gens
        -- sort by length
        equiv' = List.sortBy compare' (map (\cls -> (length cls, cls)) equiv)
    in
      map snd equiv'
    where
      -- increasing order
      compare' (len, _) (len', _) = compare len' len 

                                    
-- Remove first occurence of generator and substitute
rmvGenFn :: (BasicVar b) => (b, Gen b) -> Fn b -> Fn b
rmvGenFn (x, gen) = go
    where
      go (Dens dist pt es) = Dens dist pt es
      go (Ind fn cond) = Ind (go fn) cond
      go (Let y e fn) = Let y e (go fn)
      go (Prod fn1 fn2) = Prod (go fn1) (go fn2)
      go (Pi x' gen' fn)
        | gen =\= gen' = subst x' (Var x) fn
        | otherwise = Pi x' gen' (go fn)

                      
factor' :: (BasicVar b) => [Fn b] -> Fn b
factor' fns =
      go (genEquivClass (concat (map gatherGen fns)))
    where
      go [] = prodFn fns
      go (equivCls:rest)
          | length equivCls >= 1 =
              let (facs, unfacs) = List.partition (\fn -> (any (\(_, gen) -> containsGen gen fn) equivCls)) fns
              in
                if length facs > 0
                then
                    let (x, gen) = head equivCls
                        facs' = map (rmvGenFn (x, gen)) facs
                    in                      
                      prodFn (Pi x gen (factor' facs') : unfacs)
                else go rest
          | otherwise = prodFn fns

factorM' :: (Monad m, BasicVar b) => [Fn b] -> m (Fn b)
factorM' fns =
      do -- let equivClass = genEquivClass (concat (map gatherGen fns))
         -- traceM "" $ "EquivClasses: " ++ render (vcat (map (\clss -> sepBy commasp clss) equivClass))
         go (genEquivClass (concat (map gatherGen fns)))
    where
      go [] = return $ prodFn fns
      go (equivCls:rest)
          | length equivCls >= 1 =
              let (facs, unfacs) = List.partition (\fn -> (any (\(_, gen) -> containsGen gen fn) equivCls)) fns
              in
                if length facs > 0
                then
                    let (x, gen) = head equivCls
                        facs' = map (rmvGenFn (x, gen)) facs
                    in                      
                      do -- barFn <- factorM' facs'
                         -- traceM $ "INNER: " ++ pprShow barFn
                         return $ prodFn (Pi x gen (factor' facs') : unfacs)
                else
                    do traceM $ "Continuing facs: " ++ pprShowLs facs ++ " and unfacs: " ++ pprShowLs unfacs
                       go rest
          | otherwise =
              return $ prodFn fns

factorM :: (Monad m, BasicVar b) => Fn b -> m (Fn b)
factorM fn =
    factorM' (unfactor fn)
                     
factor :: (TypedVar b Typ) => Fn b -> Fn b
factor = factor' . unfactor


         
-----------------------------------
-- == Mixture factoring
        
type CatCtx b = Map.Map b Int -- Cat variable and number of index
type MixM b m = RWST (MixRdr b) [()] (MixSt b) m
data MixSt b = MS { ms_used :: Set.Set b
                  , ms_conds :: [IndCond b] }
data MixRdr b = MR { mr_catCtx :: CatCtx b
                   , mr_locCatCtx :: Map.Map b (Exp b)
                   , mr_pt :: Exp b }


chkDensPt :: (BasicVar b, Monad m) => Exp b -> MixM b m ()
chkDensPt e =
    do pt <- asks mr_pt
       let v = densPtVar pt
           idxs = densPtIdx pt
       case e of
         Proj (Var y) es ->
             if v == y
             then mapM_ unify (zip idxs es)
             else return ()
         _ -> return ()
    where
      unify (idx1, Var idx2) =
          if idx1 == idx2
          then return ()
          else
              do locCatCtx <- asks mr_locCatCtx
                 case Map.lookup idx2 locCatCtx of
                   Just _ ->
                       do modify (\st -> st { ms_conds = CatCond idx1 (Var idx2) : (ms_conds st)} )
                          modify (\st -> st { ms_used = Set.insert idx1 (ms_used st) })
                   Nothing -> return ()
      unify _ = return ()

                
withLocCatCtx :: (BasicVar b, Monad m) => b -> Exp b -> MixM b m a -> MixM b m a
withLocCatCtx x e comp =
    do catCtx <- asks mr_catCtx
       case e of
         Var y ->
             if isCat catCtx y []
             then local (\rdr -> rdr { mr_locCatCtx = Map.insert x e (mr_locCatCtx rdr) }) comp
             else comp
         Proj (Var y) es ->
             if isCat catCtx y es
             then local (\rdr -> rdr { mr_locCatCtx = Map.insert x e (mr_locCatCtx rdr) }) comp
             else comp
         _ -> comp
    where
      isCat catCtx v es =
          case Map.lookup v catCtx of
            Just lenIdxs -> lenIdxs == length es
            Nothing -> False

condify :: (BasicVar b) => Fn b -> [IndCond b] -> Fn b
condify fn conds =
    case conds of
      [] -> fn
      _ -> Ind fn conds

                       
mix :: (BasicVar b, Monad m) => Fn b -> MixM b m (Fn b)
mix (Dens dist pt es) = return $ Dens dist pt es
mix (Ind fn conds) =
    do fn' <- mix fn
       return $ Ind fn' conds
mix (Let x e fn) =
    do saved <- gets ms_conds
       modify (\st -> st { ms_conds = [] })
       chkDensPt e
       fn' <- withLocCatCtx x e (mix fn)
       conds <- gets ms_conds
       modify (\st -> st { ms_conds = saved })
       return $ Let x e (condify fn' conds)
mix (Prod fn1 fn2) =
    do fn1' <- mix fn1
       fn2' <- mix fn2
       return $ Prod fn1' fn2'
mix (Pi x gen fn) =
    do saved <- gets ms_conds
       modify (\st -> st { ms_conds = [] })
       fn' <- mix fn
       conds <- gets ms_conds
       modify (\st -> st { ms_conds = saved })
       return $ Pi x gen (condify fn' conds)

              
runMix' :: (BasicVar b, Monad m) => CatCtx b -> Fn b -> Fn b -> m (Fn b)
runMix' catCtx fnPt fn =
    do let rdr = MR catCtx Map.empty (gatherDensPt fnPt)
           st = MS Set.empty []
           gens = gatherGen fnPt
       (v, MS used _, _) <- runRWST (mix fn) rdr st
       let fn' = foldl (\acc (idx, gen) ->
                            if Set.member idx used
                            then Pi idx gen acc
                            else acc) v gens
       return fn'

              
runMix :: (BasicVar b) => CatCtx b -> Fn b -> Fn b -> Fn b
runMix catCtx fnPt fn = runIdentity (runMix' catCtx fnPt fn)


unMixArgs :: (BasicVar b) => Fn b -> [Exp b]
unMixArgs = go' . go
    where
      go' (Dens _ _ es) = es
      go' (Let x e fn) = go' (subst x e fn)
      go' fn@(Ind _ _) =
          error $ "[RwCore] @unMixArgs | Shouldn't happen " ++ pprShow fn
      go' fn@(Prod _ _) =
          error $ "[RwCore] @unMixArgs | Shouldn't happen " ++ pprShow fn
      go' (Pi _ _ fn) = go' fn
                
      go fn@(Dens _ _ _) = fn
      go (Let x e fn) = Let x e (go fn)
      go (Ind fn conds) =
          go (foldl (\acc cond ->
                         case cond of
                           CatCond x e -> substAExpFn e (Var x) acc
                    ) fn conds)
      go fn@(Prod _ _) =
          error $ "[RwCore] @ | Shouldn't happen " ++ pprShow fn
      go (Pi _ _ fn) = go fn

                       
unMixArgs' :: (BasicVar b) => Fn b -> Fn b
unMixArgs' = go
    where
      go fn@(Dens _ _ _) = fn
      go (Let x e fn) = Let x e (go fn)
      go (Ind fn conds) =
          go (foldl (\acc cond ->
                         case cond of
                           CatCond x e -> substAExpFn (Var x) e acc
                    ) fn conds)
      go fn@(Prod _ _) =
          error $ "[RwCore] @ | Shouldn't happen " ++ pprShow fn
      go (Pi _ _ fn) = go fn


collapseFn :: (BasicVar b) => Fn b -> Fn b
collapseFn fn@Dens{} = fn
collapseFn (Ind fn conds) =
    foldl (\acc cond ->
               case cond of
                 CatCond x e -> subst x e acc
          ) fn conds
collapseFn (Let x e fn) = collapseFn (subst x e fn)
collapseFn (Prod fn1 fn2) = Prod (collapseFn fn1) (collapseFn fn2)
collapseFn (Pi x gen fn) = Pi x gen (collapseFn fn)
