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

module Low.LinLow
    ( runLinDecl
    , runLinDecl'
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Var
import Compile.CompData
import Low.LowSyn


----------------------------------------------------------------------
-- = LinLow description
{-| [Note]

Linearize expressions in a Low declaration.

-}


-----------------------------------
-- == Types and operations

type LinM = ReaderT LinRdr CompM
type LinM' b = WriterT [Assign b] LinM
data LinRdr = LR { lr_linProj :: Bool
                 , lr_genSym :: GenSym }

liftLinM :: IO a -> LinM a
liftLinM c = liftIO $ c
            
liftLinM' :: IO a -> LinM' b a
liftLinM' c = lift $ liftLinM $ c
    
freshId :: (TypedVar b t) => LinM' b b
freshId =
    do genSym <- asks lr_genSym
       liftLinM' (mkIdIO genSym Anon Local)

emitAssign :: Assign b -> LinM' b ()
emitAssign assign = tell [assign]
           

                    
-----------------------------------
-- == Transformation
                    
bindExp :: (TypedVar b t) => Exp b -> (Exp b -> LinM' b (Exp b)) -> LinM' b (Exp b)
bindExp e k =
    do v <- freshId
       emitAssign (v, e)
       k (Var v)

         
linExpK :: (TypedVar b t) => Exp b -> (Exp b -> LinM' b (Exp b)) -> LinM' b (Exp b)
linExpK (Var x) k = k (Var x)
linExpK (Lit lit) k = k (Lit lit)
linExpK (Call ce es) k =
    case es of
      [] -> bindExp (Call ce []) k
      _ -> linExpsK es (\es' -> bindExp (Call ce es') k)
linExpK (DistOp dop dist dm es) k =
    linExpsK es (\es' -> bindExp (DistOp dop dist dm es') k)
linExpK (Proj e es) k =
    do fLinProj <- asks lr_linProj
       if fLinProj 
       then linExpK e (\e' -> linExpsK es (\es' -> linProj e' (reverse es') k))
       else linExpK e (\e' -> linExpsK es (\es' -> bindExp (Proj e' es') k))
    where
      linProj acc (e':[]) k' =
          bindExp (Proj acc [e']) k'
      linProj acc (e':es') k' =
          linProj acc es' (\acc' -> bindExp (Proj acc' [e']) k')
      linProj _ [] _ = error $ "[LinLow] @linExpK | Projection has empty list"

                       
linExpsK :: (TypedVar b t) => [Exp b] -> ([Exp b] -> LinM' b (Exp b)) -> LinM' b (Exp b)
linExpsK es k =
    case es of
      e:[] -> linExpK e (\e' -> k [e'])
      e:es' -> linExpK e (\e' -> linExpsK es' (\es'' -> k (e':es'')))
      [] -> error $ "[LinLow] @linExpsK | Shouldn't call with empty list"

      
runLinExp :: (TypedVar b t) => Exp b -> LinM ([Assign b], Exp b)
runLinExp e =
    do (e', assigns) <- runWriterT (chkHd e)
       return (assigns, e')
    where  
      chkHd (Var x) = return $ Var x
      chkHd (Lit lit) = return $ Lit lit
      chkHd (Call ce es) =
          case es of
            [] -> return $ Call ce es
            _ -> linExpsK es (\es' -> return $ Call ce es')
      chkHd (DistOp dop dist dm es) =
          linExpsK es (\es' -> return $ DistOp dop dist dm es')
      chkHd (Proj ept es) =
          linExpK ept (\e' -> linExpsK es (\es' -> return $ Proj e' es'))

                  
runLinExp' :: (TypedVar b t) => Exp b -> LinM ([Assign b], Exp b)
runLinExp' e =
    do (e', assigns) <- runWriterT (linExpK e return)
       return (assigns, e')

              
linGen :: (TypedVar b t) => Gen b -> LinM ([Assign b], Gen b)
linGen (Until e1 e2) =
    do (assigns1, e1') <- runLinExp' e1
       (assigns2, e2') <- runLinExp' e2
       return $ (assigns1 ++ assigns2, Until e1' e2')

              
linStmt :: (TypedVar b t) => Stmt b -> LinM (Stmt b)
linStmt Skip = return Skip
linStmt (Exp e) =
    do (assigns, e') <- runLinExp e
       return $ assignsToStmt' assigns (Exp e')
linStmt (Assign x e) =
    do (assigns, e') <- runLinExp e
       return $ assignsToStmt' assigns (Assign x e')
linStmt (Store x es uk e) =
    do aes' <- mapM runLinExp' es
       let (assignss, es') = unzip aes'
       (assigns, e') <- runLinExp e
       return $ assignsToStmt' (concat assignss ++ assigns) (Store x es' uk e')
linStmt (Seq s1 s2) =
    do s1' <- linStmt s1
       s2' <- linStmt s2
       return $ Seq s1' s2'
linStmt (If e s1 s2) =
    do (assigns, e') <- runLinExp' e
       s1' <- linStmt s1
       s2' <- linStmt s2
       return $ assignsToStmt' assigns (If e' s1' s2')
linStmt (Loop lk x gen s) =
    do (assigns, gen') <- linGen gen
       s' <- linStmt s       
       return $ assignsToStmt' assigns (Loop lk x gen' s')
linStmt (MapRed acc x gen s e) =
    do (assignsg, gen') <- linGen gen
       s' <- linStmt s
       (assignse, e') <- runLinExp' e
       let s'' = Seq s' (assignsToStmt assignse)
       return $ assignsToStmt' assignsg (MapRed acc x gen' s'' e')

              
linDecl :: (TypedVar b t) => Decl b -> LinM (Decl b)
linDecl (Fun name params alloc body retExp retTy) =
    do body' <- linStmt body
       (assigns, retExp') <- linRet retExp
       let body'' = Seq body' (assignsToStmt assigns)
       return $ Fun name params alloc body'' retExp' retTy
    where
      linRet (Just e) = runLinExp e >>= \(assigns, e') -> return (assigns, Just e')
      linRet Nothing = return ([], Nothing)


                       
-----------------------------------
-- == Top-level

runLinDecl :: (TypedVar b t) => GenSym -> Bool -> Decl b -> CompM (Decl b)
runLinDecl genSym linProj decl =
    runReaderT (linDecl decl) (LR linProj genSym)



runLinDecl' :: (TypedVar b t) => GenSym -> Bool -> Decl b -> IO (Decl b)
runLinDecl' genSym linProj decl =
    do v <- runExceptT (runLinDecl genSym linProj decl)
       case v of
         Left errMsg -> error $ "[LinLow] @runLinDecl' | Shouldn't happen: " ++ errMsg
         Right decl' -> return $ decl'
