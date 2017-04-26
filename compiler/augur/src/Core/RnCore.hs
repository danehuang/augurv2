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

module Core.RnCore
    ( runRnModDeclsTyVar
    , runRnFnTyVar
    , runRnKernUTyVar ) where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.RWS
import Data.Traversable (traverse)
import Debug.Trace

import AstUtil.Fresh
import AstUtil.Var
import AstUtil.Pretty
import Compile.CompData
import Core.DensSyn
import Core.CoreSyn
import Core.KernSyn
import Core.CoreTySyn
    
----------------------------------------------------------------------
-- = RnCore Description
{-| [Note]

Rename the variables in a Core program, i.e., alpha convert.

-}



-----------------------------------
-- == Types and operations

type RnM b = RWST (RnRdr b) [()] (Map.Map String b) CompM
data RnRdr b = RR { rr_varsM :: Map.Map String b
                  , rr_genSym :: GenSym }

withVars :: (Show b, BasicVar b) => Map.Map String b -> RnM b a -> RnM b a
withVars varsM = local (\rdr -> rdr { rr_varsM = varsM })

freshId :: (Show b, BasicVar b) => String -> IdKind -> RnM b b
freshId x ik =
    do genSym <- asks rr_genSym
       lift $ lift $ mkIdIO genSym (mkName x) ik


            
-----------------------------------
-- == Transformation
            
rnId :: (Show b, BasicVar b) => String -> RnM b b
rnId x =
    do varsM <- asks rr_varsM
       case Map.lookup x varsM of
         Just x' -> return x'
         Nothing -> throwError $ "[Core.RnCore] @convId | lookup of " ++ show x ++ " failed in ctx: " ++ show varsM

                    
rnExp :: (Show b, BasicVar b) => Exp String -> RnM b (Exp b)
rnExp e = traverse rnId e

          
rnGen :: (Show b, BasicVar b) => Gen String -> RnM b (Gen b)
rnGen gen = traverse rnId gen

            
rnIndCond :: (Show b, BasicVar b) => IndCond String -> RnM b (IndCond b)
rnIndCond cond = traverse rnId cond
                 
            
rnFn :: (Show b, BasicVar b) => Fn String -> RnM b (Fn b)
rnFn (Dens dist pt es) =
    do pt' <- rnExp pt
       es' <- mapM rnExp es
       return $ Dens dist pt' es'
rnFn (Ind fn conds) =
    do fn' <- rnFn fn
       conds' <- mapM rnIndCond conds
       return $ Ind fn' conds' 
rnFn (Let x e fn) =
    do e' <- rnExp e
       x' <- freshId x Local
       varsM <- asks rr_varsM
       fn' <- withVars (Map.insert x x' varsM) (rnFn fn)
       return $ Let x' e' fn'
rnFn (Prod fn1 fn2) =
    do fn1' <- rnFn fn1
       fn2' <- rnFn fn2
       return $ Prod fn1' fn2' 
rnFn (Pi x gen fn) =
    do gen' <- rnGen gen
       x' <- freshId x Local
       varsM <- asks rr_varsM
       fn' <- withVars (Map.insert x x' varsM) (rnFn fn)
       return $ Pi x' gen' fn'

              
rnKernUnit :: (Show b, BasicVar b) => KernUnit String -> RnM b (KernUnit b)
rnKernUnit (Single x) =
    do x' <- rnId x
       return $ Single x'
rnKernUnit (Block xs) =
    do xs' <- mapM rnId xs
       return $ Block xs'


rnUserCode :: (Show b, BasicVar b) => UserCode String -> RnM b (UserCode b)
rnUserCode Empty = return Empty
rnUserCode (Proposal e grid) =
    do traceM $ "HERE: " ++ show e
       foo <- mapM (\(v, gen) ->
                        do v' <- freshId v Local
                           varsM <- asks rr_varsM
                           gen' <- withVars (Map.insert v v' varsM) (rnGen gen)
                           return (v, v', gen')
                   ) grid
       let grid' = map (\(_, v', gen') -> (v', gen')) foo
           varsM' = Map.fromList (map (\(v, v', _) -> (v, v')) foo)
       varsM <- asks rr_varsM
       e' <- withVars (varsM `Map.union` varsM') (rnExp e)
       return $ Proposal e' grid'


rnKernKind :: (Show b, BasicVar b) => KernKind (UserCode String) -> RnM b (KernKind (UserCode b))
rnKernKind (UserProp pk) =
    case pk of
      Joint code ->
          do code' <- rnUserCode code
             return $ UserProp (Joint code')
      MWG code1 code2 code3 code4 ->
          do code1' <- rnUserCode code1
             code2' <- rnUserCode code2
             code3' <- rnUserCode code3
             code4' <- rnUserCode code4
             return $ UserProp (MWG code1' code2' code3' code4')
rnKernKind (GradProp gk) =
    case gk of
      HMC code1 code2 simLen stepSize ->
          do code1' <- rnUserCode code1
             code2' <- rnUserCode code2
             return $ GradProp (HMC code1' code2' simLen stepSize)
      NUTS code1 code2 stepSize ->
          do code1' <- rnUserCode code1
             code2' <- rnUserCode code2
             return $ GradProp (NUTS code1' code2' stepSize)
      Reflect code1 code2 simLen stepSize ->
          do code1' <- rnUserCode code1
             code2' <- rnUserCode code2
             return $ GradProp (Reflect code1' code2' simLen stepSize)
rnKernKind (Gibbs gk) =
    case gk of
      Disc code ->
          do code' <- rnUserCode code
             return $ Gibbs (Disc code')
      Conj code1 code2 ->
          do code1' <- rnUserCode code1
             code2' <- rnUserCode code2
             return $ Gibbs (Conj code1' code2')
rnKernKind (Slice sk) =
    case sk of
      Ellip code1 code2 ->
          do code1' <- rnUserCode code1
             code2' <- rnUserCode code2
             return $ Slice (Ellip code1' code2')

             
rnKernU :: (Show b, BasicVar b) => KernU String -> RnM b (KernU b)
rnKernU (Base kind ku fn allocs kernParams code) =
    do extra' <- mapM (\(v, ak) -> rnId v >>= \v' -> return (v', ak)) allocs
       kind' <- rnKernKind kind
       ku' <- rnKernUnit ku
       fn' <- rnFn fn
       code' <- rnUserCode code
       kernParams' <- mapM rnId kernParams
       return $ Base kind' ku' fn' extra' kernParams' code'
rnKernU (Tensor k1 k2) =
    do k1' <- rnKernU k1
       k2' <- rnKernU k2
       return $ Tensor k1' k2'

              
-----------------------------------
-- == Top-level

runRnModDeclsTyVar :: CompInfo -> ModDecls String -> IO (Map.Map String (TVar Typ), ModDecls (TVar Typ))
runRnModDeclsTyVar cinfo modDecls =
    do let genSym = getGenSym cinfo
       modDecls' <- mapM (\(ik, x, ty) ->
                              do x' <- mkIdIO genSym (mkName x) ik
                                 return (x, ik, x', ty)
                         ) modDecls
       let modDecls'' = map (\(_, ik, x', ty) -> (ik, setType x' ty, ty)) modDecls'
           varsM = Map.fromList (map (\(x, _, x', _) -> (x, x')) modDecls')
       return (varsM, modDecls'')


 
runRnFnTyVar :: CompInfo -> Map.Map String (TVar Typ) -> ModDecls (TVar Typ) -> Fn String -> CompM (Fn (TVar Typ))
runRnFnTyVar cinfo varsM modDecls fn =
    do let genSym = getGenSym cinfo
       debugM "[Core.RnCore]" $ "@runRnFnTyVar | Input (Density):\n" ++ pprShow fn
       (fn', _, _) <- runRWST (rnFn fn) (RR varsM genSym) varsM
       debugM "[Core.RnCore]" $ "@runRnFnTyVar | Output (Density):\n" ++ pprShow fn'
       return fn'
              
              
runRnKernUTyVar :: CompInfo -> Map.Map String (TVar Typ) -> ModDecls (TVar Typ) -> KernU String -> CompM (KernU (TVar Typ))
runRnKernUTyVar cinfo varsM modDecls kern =
    do debugM "[Core.RnCore]" $ "@runRnKernUTyVar | Input (KernU):\n" ++ pprShow kern
       (kern', _, _) <- runRWST (rnKernU kern) (RR varsM (getGenSym cinfo)) varsM
       debugM "[Core.RnCore]" $ "@runRnKernUTyVar | Output (KernU):\n" ++ pprShow kern'
       return kern'
