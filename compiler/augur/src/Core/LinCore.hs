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

module Core.LinCore where

import Control.Monad.Reader
import Control.Monad.Writer
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Var
import Core.DensSyn
import Core.KernSyn
import Compile.CompData
    

----------------------------------------------------------------------
-- = LinCore Description
{-| [Note]

Linearize expressions in a Core declaration.

We do not linearize:
1) Projections
2) Generator expressions

-}


-----------------------------------
-- == Types and operations

type LinM b = WriterT [Assign b] LinM'
type LinM' = ReaderT GenSym CompM
type Assign b = (b, Exp b)
    
freshId :: (TypedVar b t) => LinM b b
freshId =
    do genSym <- ask
       lift $ lift $ lift $ mkIdIO genSym Anon Local
    
emitAssign :: Assign b -> LinM b ()
emitAssign assign = tell [assign]

assignsToFn :: [Assign b] -> Fn b -> Fn b
assignsToFn assigns fn =
    case assigns of
      [] -> fn
      _ -> foldl (\acc (x, e) -> Let x e acc) fn assigns 
                    

                                       
-----------------------------------
-- == Transformation

bindExp :: (TypedVar b t) => Exp b -> (Exp b -> LinM b (Exp b)) -> LinM b (Exp b)
bindExp e k =
    do v <- freshId
       emitAssign (v, e)
       k (Var v)

         
linExpK :: (TypedVar b t) => Exp b -> (Exp b -> LinM b (Exp b)) -> LinM b (Exp b)
linExpK (Var x) k = k (Var x)
linExpK (Lit lit) k = k (Lit lit)
linExpK (Call ce es) k =
    case es of
      [] -> bindExp (Call ce []) k
      _ -> linExpsK es (\es' -> bindExp (Call ce es') k)
linExpK (DistOp dop dist es) k =
    linExpsK es (\es' -> bindExp (DistOp dop dist es') k)
linExpK (Proj e es) k =
    linExpK e (\e' -> linExpsK es (\es' -> bindExp (Proj e' es') k))
    {-
    linExpK e (\e' -> linExpsK es (\es' -> linProj e' (reverse es') k))    
    where
      linProj acc (e':[]) k' =
          bindExp (Proj acc [e']) k'
      linProj acc (e':es') k' =
          linProj acc es' (\acc' -> bindExp (Proj acc' [e']) k')
      linProj _ [] _ = error $ "@linExpK | Projection has empty list"
     -}

                       
linExpsK :: (TypedVar b t) => [Exp b] -> ([Exp b] -> LinM b (Exp b)) -> LinM b (Exp b)
linExpsK es k =
    case es of
      e:[] -> linExpK e (\e' -> k [e'])
      e:es' -> linExpK e (\e' -> linExpsK es' (\es'' -> k (e':es'')))
      [] -> error $ "@linExpsK | Shouldn't call with empty list"

runLinExp :: (TypedVar b t) => Exp b -> LinM' ([Assign b], Exp b)
runLinExp e =
    do -- (e', assigns) <- runWriterT (linExpK e return)
       (e', assigns) <- runWriterT (chkHd e)
       return (reverse assigns, e')
    where
      chkHd (Var x) = return $ Var x
      chkHd (Lit lit) = return $ Lit lit
      chkHd (Call ce es) =
          case es of
            [] -> return $ Call ce es
            _ -> linExpsK es (\es' -> return $ Call ce es')
      chkHd (DistOp dop dist es) =
          linExpsK es (\es' -> return $ DistOp dop dist es')
      chkHd (Proj ept es) =
          linExpK ept (\e' -> linExpsK es (\es' -> return $ Proj e' es'))

runLinExp' :: (TypedVar b t) => Exp b -> LinM' ([Assign b], Exp b)
runLinExp' e =
    do (e', assigns) <- runWriterT (linExpK e return)
       return (reverse assigns, e')

                  
runLinGen :: (TypedVar b t) => Gen b -> LinM' ([Assign b], Gen b)
runLinGen (Until e1 e2) =
    do (assigns1, e1') <- runLinExp e1
       (assigns2, e2') <- runLinExp e2
       return $ (reverse assigns1 ++ reverse assigns2, Until e1' e2')

                 
runLinIndCond :: (TypedVar b t) => IndCond b -> LinM' ([Assign b], IndCond b)
runLinIndCond (CatCond x e) =
    do (assigns, e') <- runLinExp e
       return (reverse assigns, CatCond x e')
              
linFnK :: (TypedVar b t) => Fn b -> (Fn b -> LinM' (Fn b)) -> LinM' (Fn b)
linFnK (Dens dist ept es) k =
    do (assignss, es') <- mapM runLinExp' es >>= return . unzip       
       k (assignsToFn (concat assignss) (Dens dist ept es'))
linFnK (Ind fn conds) k =
    do (assigns, conds') <- mapM runLinIndCond conds >>= return . unzip
       linFnK fn (\fn' -> k (assignsToFn (concat assigns) (Ind fn' conds')))
linFnK (Let x e fn) k =
    do (assigns, e') <- runLinExp e
       linFnK fn (\fn' -> k (assignsToFn assigns (Let x e' fn')))
linFnK (Prod fn1 fn2) k =
    linFnK fn1 (\fn1' -> linFnK fn2 (\fn2' -> k (Prod fn1' fn2'))) 
linFnK (Pi x gen fn) k =
    do {-
       (assigns, gen') <- runLinGen gen
       linFnK fn (\fn' -> k (assignsToFn assigns (Pi x gen' fn')))
        -}
       linFnK fn (\fn' -> k (Pi x gen fn'))

              
-----------------------------------
-- == Top-level

runLinFn :: (TypedVar b t) => GenSym -> Fn b -> CompM (Fn b)
runLinFn genSym fn =
    runReaderT (linFnK fn return) genSym

               
runLinKernU :: (TypedVar b t) => GenSym -> KernU b -> CompM (KernU b)
runLinKernU genSym k =
    case k of
      Base kind ku fn extra kernParams code ->
          do fn' <- runLinFn genSym fn
             return $ Base kind ku fn' extra kernParams code
      Tensor k1 k2 ->
          do k1' <- runLinKernU genSym k1
             k2' <- runLinKernU genSym k2
             return $ Tensor k1' k2'
