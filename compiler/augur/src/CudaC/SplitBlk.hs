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
import Data.Maybe
import Debug.Trace
    
import AstUtil.Var
import AstUtil.Fresh
import AstUtil.VarOp
import AstUtil.Pretty
import CudaC.BlkSyn
import CudaC.RtSize
import Compile.CompData
import Comm.Prim
import Core.CoreTySyn
import Core.CoreSyn
import Low.DepLow
import Low.LowSyn
import Low.LowpPrimSyn
import Low.LintLow
import qualified Low.LowShpSyn as S
   

----------------------------------------------------------------------
-- = SplitBlk Description
{-| [Note]

Split a computational unit so that:
1) Can commute order of loops based on runtime values
2) Can convert to summation block

-}

-----------------------------------
-- == Types and operations

type LoopM b = RWST (LoopRdr b) [()] (LoopSt b) CompM
data LoopRdr b =
    LR { lr_splitOnLoop :: Bool 
       , lr_splitOnAtmInc :: Bool }
data LoopSt b =
    LS { ls_hist :: [Stmt b]
       , ls_split :: [Stmt b]
       , ls_localCtx :: Map.Map b (Exp b) }

    
withSplitOnLoop :: (TypedVar b Typ) => Bool -> LoopM b a -> LoopM b a
withSplitOnLoop splitOnLoop =
    local (\rdr -> rdr { lr_splitOnLoop = splitOnLoop })

          
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

        
splitHist :: (TypedVar b Typ) => Stmt b -> LoopM b ()
splitHist stmt =
    do (hist, localCtx) <- saveCtx
       stmts <- currHist
       let s = seqStmt (reverse (stmt : stmts))
           (genSet, killSet) = gkStmt s
           deps = Set.toList (genSet `Set.difference` killSet)
           s' = seqStmt [ collectLocal deps localCtx, s ]
       emitSplit (seqStmt (reverse (s' : hist)))
       clearHist
       

localUp :: (TypedVar b Typ) => Stmt b -> Bool
localUp Skip = False
localUp (Exp _) = False
localUp (Assign _ _) = False
localUp (Store x _ _ _) = isLocalId x
localUp (Seq s1 s2) = localUp s1 || localUp s2
localUp (If _ s1 s2) = localUp s1 || localUp s2
localUp (Loop _ _ _ s) = localUp s
localUp (MapRed _ _ _ s _) = localUp s


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
    do splitOnLoop <- asks lr_splitOnLoop
       if splitOnLoop
       then do (hist, localCtx) <- saveCtx
               withSplitOnLoop False (splitLoop s)
               stmts <- currHist
               if not (localUp s)
               then do let s' = Loop lk x gen (seqStmt (reverse stmts))
                           (genSet, killSet) = gkStmt s'
                           deps = Set.toList (Set.delete x (genSet `Set.difference` killSet))
                           s'' = seqStmt [ collectLocal deps localCtx, s' ]
                       emitSplit (seqStmt (reverse (s'' : hist)))
                       clearHist
               else do restoreCtx hist localCtx
                       emitHist (Loop lk x gen (seqStmt (reverse stmts)))    
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


runSplitLoop :: (TypedVar b Typ) => Bool -> Stmt b -> CompM [Stmt b]
runSplitLoop splitOnStore s =
    do let initRdr = LR True splitOnStore
           initSt = LS [] [] Map.empty
       (_, LS hist split localCtx, _) <- runRWST (splitLoop s) initRdr initSt
       return $ appLast split localCtx hist
    where
      appLast split localCtx hist = 
          case hist of
            [] -> reverse split
            [Skip] -> reverse split
            _ ->
                let s = seqStmt (reverse hist)
                    -- (genSet, killSet) = gkStmt s
                    -- deps = Set.toList (genSet `Set.difference` killSet)
                    -- s' = seqStmt [ collectLocal deps localCtx, s ]
                in
                  reverse (s : split)
                
-----------------------------------
-- == Commuting


{-| [Note]

"loop head normal form": A bunch of pure expressions before a loop.

x_1 = e_1; 
...
x_n = e_n; 
loop (i <- gen[e_1 / x_1, ..., e_n / x_n]) {
  s[e_1 / x_1, ..., e_n / x_n]
}
-}
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


tryCommComp :: (TypedVar b Typ) => CompInfo -> InferCtx b -> Map.Map b Int -> Map.Map b Typ -> Comp b -> CompM (Bool, Comp b)
tryCommComp cinfo inferCtx rtSizeCtx openCtx comp =
    go comp
    where
      go (Single sc s) = return (False, Single sc s)
      go (Block lk x gen s) =
          case colHdNrmStmt s of
            Just (Loop lk' x' gen' s') ->                
                case compareGen rtSizeCtx gen gen' of
                  RtLt -> 
                      do let openCtx' = Map.insert x' IntTy (Map.insert x IntTy openCtx)
                         s'' <- runLintStmt cinfo True inferCtx openCtx' s'
                         return (True, Block lk' x' gen' (Loop lk x gen s''))
                  _ -> return (False, Block lk x gen s)
            _ -> return (False, Block lk x gen s)          
      go (Reduce ctx acc x gen s e) = return (False, Reduce ctx acc x gen s e)


            
-----------------------------------
-- == Top-level
              
runSplitComp :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> Map.Map b Int -> Map.Map b Typ -> Comp b -> CompM [Comp b]
runSplitComp cinfo copt inferCtx rtSizeCtx openCtx comp =
    do debugM "[CudaC.SplitBlk]" $ "@runSplitComp | Input: " ++ pprShow comp ++ " with ctx: " ++ pprShow rtSizeCtx      
       comps <- splitComp comp
       (chgs, comps') <- mapM (tryCommComp cinfo inferCtx rtSizeCtx openCtx) comps >>= return . unzip
       debugM "[CudaC.SplitBlk]" $ "@runSplitComp | Changes " ++ show chgs ++ " with split\n" ++ pprShowLs comps'
       if any (\x -> x) chgs
       then return comps'
       else return [ comp ]
    where
      splitComp (Single sc s) =
          return [ Single sc s ]
      splitComp (Block lk x gen s) =
          do split <- runSplitLoop (getSplitOnAtmInc copt) s
             mapM (\s' -> return $ Block lk x gen s') split
      splitComp (Reduce ctx acc x gen s e) =
          return [ Reduce ctx acc x gen s e ]



-----------------------------------
-- == Split AtmInc into Sum blocks

data AtmIncKind b = Scalar b (Exp b)
                  | Vector b (Exp b) b

splitAtmIncStmt :: (TypedVar b Typ) => GenSym -> S.ShpCtx b -> Map.Map b Int -> b -> Gen b -> Stmt b -> CompM (Either (Stmt b) (AtmIncKind b, Stmt b, Stmt b))
splitAtmIncStmt genSym shpCtx rtSizeCtx idx gen s =
    do let (stmts1, stmts2) = break isAtmInc (splat s)
       if length stmts2 > 0
       then
          do let (genSet, killSet) = gkStmt (unsplat (tail stmts2))
                 deps = Set.toList (genSet `Set.difference` killSet)
                 localCtx = Map.fromList (concat (map getLocal stmts1))
                 s1 = unsplat stmts1                 
                 s2 = seqStmt [ collectLocal deps localCtx, (unsplat (tail stmts2)) ]
             (aik, s_up) <- getAtmInc (head stmts2)
             case aik of
               Scalar _ _ -> return $ Right (aik, Seq s1 s_up, s2)
               Vector y _ _ ->
                   do case chkSplit y of
                        RtLt -> return $ Right (aik, Seq s1 s_up, s2)
                        _ -> return $ Left s
       else
           return $ Left s
    where
      chkSplit x =
          case Map.lookup x shpCtx of
            Just (S.SingConn (S.Cpy y) S.Scalar) ->
                compareGenExp rtSizeCtx (Call (PrimId DM_Mem PM_Fn SizeVec) [ Var y ]) gen
            _ -> RtIncomp
      
      getAtmInc (LAtm (Store x [] AtmInc e)) =
          case getType' x of
            IntTy -> return (Scalar x e, Skip)
            RealTy -> return (Scalar x e, Skip)
            VecTy IntTy ->
                do g <- lift $ mkTyIdIO genSym Anon ModAux (VecTy (VecTy RealTy))
                   idx' <- lift $ mkTyIdIO genSym Anon Local IntTy
                   g' <- lift $ mkTyIdIO genSym Anon Local (VecTy RealTy)
                   let s0 = Assign g' (Proj (Var g) [Var idx'])
                       s1 = Store g' [Var idx] Update (Proj e [Var idx'])
                       gen' = Until (Lit (Int 0)) (Call (PrimId DM_Mem PM_Fn SizeVec) [ Var x ])
                       s2 = Loop Parallel idx' gen' (seqStmt [s0, s1])
                   return (Vector x e g, s2)
            VecTy RealTy ->
                do g <- lift $ mkTyIdIO genSym Anon ModAux (VecTy (VecTy RealTy))
                   idx' <- lift $ mkTyIdIO genSym Anon Local IntTy
                   g' <- lift $ mkTyIdIO genSym Anon Local (VecTy RealTy)
                   let s0 = Assign g' (Proj (Var g) [Var idx'])
                       s1 = Store g' [Var idx] Update (Proj e [Var idx'])
                       gen' = Until (Lit (Int 0)) (Call (PrimId DM_Mem PM_Fn SizeVec) [ Var x ])
                       s2 = Loop Parallel idx' gen' (seqStmt [s0, s1])
                   return (Vector x e g, s2)
            _ -> error $ "splitAtmIncStmt | Shouldn't happen"
      getAtmInc _ = error $ "splitAtmIncStmt | Shouldn't happen"

      incTy IntTy = True
      incTy RealTy = True
      incTy (VecTy IntTy) = True
      incTy (VecTy RealTy) = True
      incTy _ = False
                    
      isAtmInc (LAtm s) =
          case s of
            Store x es AtmInc _ ->
                case es of
                  [] -> isModAux (idKind x) && incTy (getType' x)
                  _ -> False                
            _ -> False
      isAtmInc _ = False

      getLocal (LAtm s) =
          case s of
            Assign x e -> [(x, e)]
            _ -> []
      getLocal _ = []


genToShpExp' :: Gen b -> S.ShpExp b
genToShpExp' (Until _ (Var x)) = S.Val (S.Var x)
genToShpExp' _ = error $ "Shouldn't happen"

                
splitAtmIncComp' :: (TypedVar b Typ) => GenSym -> S.ShpCtx b -> Map.Map b Int -> Comp b -> CompM (Map.Map b (Typ, S.Shp b), [Comp b])
splitAtmIncComp' genSym shpCtx rtSizeCtx blk =
    case blk of
      Single sc s -> return (Map.empty, [ Single sc s ])
      Block lk x gen s ->
          do v <- splitAtmIncStmt genSym shpCtx rtSizeCtx x gen s 
             case v of
               Left s' -> return (Map.empty, [ Block lk x gen s' ])
               Right (aik, s1, s2) ->
                   case aik of
                     Scalar y e ->
                         return (Map.empty, [ Reduce Nothing (Var y) x gen s1 e, Block lk x gen s2 ])
                     Vector y e g ->
                         do let g_ty = VecTy (VecTy RealTy)
                                shp = fromJust (Map.lookup y shpCtx)
                                g_shp = S.insertShpExpEnd (genToShpExp' gen) shp
                                s_sum = Exp (Call (PrimId DM_Mem PM_Fn PllSumVec) [ Var y, Var g ])
                            return (Map.singleton g (g_ty, g_shp), [ Block lk x gen s1, Single KernelComp s_sum, Block lk x gen s2 ])
      Reduce ctx acc x gen s e -> return (Map.empty, [ Reduce ctx acc x gen s e ])
      
      
splitAtmIncComp :: (TypedVar b Typ) => GenSym -> S.ShpCtx b -> Map.Map b Int -> Comp b -> CompM (Map.Map b (Typ, S.Shp b), [Comp b])
splitAtmIncComp genSym shpCtx rtSizeCtx blk = splitAtmIncComp' genSym shpCtx rtSizeCtx blk
