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

module CudaC.RtSize where

import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace
    
import AstUtil.Var
import Low.LowSyn hiding (splat, unsplat)


----------------------------------------------------------------------
-- = RtSize Description
{-| [Note]

-}


-----------------------------------
-- == Types and Operations

type InterpM b m = ReaderT (Map.Map b Int) m


-----------------------------------
-- == Intepreter

interpGenExp :: (BasicVar b, Monad m) => Exp b -> InterpM b m (Maybe Int)
interpGenExp (Var x) =
    do rtSizeCtx <- ask
       return $ Map.lookup x rtSizeCtx
interpGenExp (Lit (Int i)) =
    return $ Just i
interpGenExp _ =
    return Nothing

                   
interpGen :: (BasicVar b, Monad m) => Gen b -> InterpM b m (Maybe Int)
interpGen (Until e1 e2) =
    do mi1 <- interpGenExp e1
       mi2 <- interpGenExp e2
       case (mi1, mi2) of
         (Just i1, Just i2) -> return $ Just (i2 - i1)
         _ -> return Nothing


              
-----------------------------------
-- == Top-level

runInterpGen :: (BasicVar b) => Map.Map b Int -> Gen b -> Maybe Int
runInterpGen rtSizeCtx gen = runIdentity (runReaderT (interpGen gen) rtSizeCtx)

                             
compareGen :: (BasicVar b) => Map.Map b Int -> Gen b -> Gen b -> Ordering
compareGen rtSizeCtx gen1 gen2 =
    case (runInterpGen rtSizeCtx gen1, runInterpGen rtSizeCtx gen2) of
      (Just i1, Just i2) -> compare i1 i2
      (Nothing, Just _) -> GT
      (Just _, Nothing) -> LT
      (Nothing, Nothing) -> EQ
