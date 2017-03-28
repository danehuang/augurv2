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

{-# LANGUAGE LambdaCase, ForeignFunctionInterface, FlexibleContexts #-}

module FFI.AugurFFI where
 
-- import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Except
import Debug.Trace
    
import AstUtil.Pretty
import Rv.RvSyn
import Core.KernSyn
import Rv.ParseRv
import qualified Compile.Compile as C
import Compile.CompData


sep :: String
sep = "$-SEP-$"

wordsWhen :: (Char -> Bool) -> String -> [Int]
wordsWhen p s =
    case dropWhile p s of
      "" -> []
      s' -> (read w) : wordsWhen p s''
          where (w, s'') = break p s'


type FFIM = ExceptT String IO
                           
parseInput :: CString -> Int -> CString -> Int -> CString -> FFIM (Model, Maybe (KernU String), [Int], Target)
parseInput sModel sTarget sInfer userMode sSizes =
    do model <- parseModel       
       kern <- parseInfer userMode
       sizes <- parseSizes
       target <- parseTarget sTarget
       return $ (model, kern, sizes, target)
    where
      parseTarget 0 = return CPU
      parseTarget 1 = return (GPU BlkStrat)
      parseTarget _ = throwError $ "ERROR" ++ sep ++ "target: " ++ show sTarget ++ " not supported"
      
      parseModel =
          do model <- lift $ peekCString sModel
             case runParseModel model of
               Left errMsg -> throwError $ "ERROR" ++ sep ++ errMsg
               Right model' -> return model'
      
      parseInfer 0 = return Nothing
      parseInfer 1 =
          do infer <- lift $ peekCString sInfer
             case runParseKer infer of
               Left msg -> throwError $ "WHAT THE HECK " ++ msg
               Right kern -> return $ Just kern
      parseInfer _ = return Nothing

      parseSizes =
          do sizes <- lift $ peekCString sSizes
             return $ wordsWhen (==',') sizes

                    
foreign export ccall hs_compile :: CString -> Int -> CString -> Int -> CString -> IO CString
hs_compile :: CString -> Int -> CString -> Int -> CString -> IO CString
hs_compile sModel target sInfer userMode sSizes =
    do v <- runExceptT (parseInput sModel target sInfer userMode sSizes)
       case v of
         Left errMsg -> newCString $ "ERROR" ++ sep ++ errMsg
         Right (model, kern, sizes, target) ->
             runComp (C.compile model kern target) >>= \case
               Left errMsg ->
                   newCString $ "ERROR" ++ sep ++ errMsg
               Right (pyModParam, pyTys, inferHdr, inferCode) ->
                   newCString $ "OK!" ++ sep ++ pprShow pyModParam ++ sep ++ pyTys ++ sep ++ inferHdr ++ sep ++ pprShow inferCode ++ sep ++ (show sizes)      
                          
