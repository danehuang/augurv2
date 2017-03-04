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

module Compile.CompData where

import Control.Monad.Except
import Debug.Trace    
-- import qualified System.Log.Logger as Log
-- import System.Log.Handler.Syslog
-- import System.Log.Handler.Simple
-- import System.Log.Handler
-- import System.Log.Formatter
-- import GHC.IO.Handle
    
import AstUtil.Fresh



----------------------------------------------------------------------
-- = CompData description
{-| [Note]

Contains data structures and operations related to compilation.

-}    


-----------------------------------
-- == Compiler monad
          
hoistExcept :: (Monad m) => Either a b -> ExceptT a m b
hoistExcept (Left l) = throwError l
hoistExcept (Right r) = return r

type CompM = ExceptT String IO

runComp :: CompM a -> IO (Either String a)
runComp = runExceptT

    
-----------------------------------
-- == Compiler information

data CompInfo =
    CompInfo { cinfo_genSym :: GenSym   -- Symbol generator
             }

getGenSym :: CompInfo -> GenSym
getGenSym = cinfo_genSym

            
-----------------------------------
-- == Compiler options
           
data CompOpt =
    CompOpt { f_lint :: Bool            -- Lint flag
            }

getLint :: CompOpt -> Bool
getLint = f_lint

runLint :: CompOpt -> a -> (a -> CompM a) -> CompM a
runLint copt v f =
    if getLint copt
    then f v
    else return v
          
runLint' :: (Monad m) => CompOpt -> Either e a -> (a -> m (Either e a)) -> m (Either e a)
runLint' copt v f =
    if getLint copt
    then case v of
           Left errMsg -> return $ Left errMsg
           Right v' -> f v'
    else return v

{-         
logM :: (Monad m) => String -> m ()
logM str = traceM str
-}

logCompileFrontend :: String
logCompileFrontend = "Compile.Frontend"

logCompileMidend :: String
logCompileMidend = "Compile.Midend"

logCompileBackend :: String
logCompileBackend = "Compile.Backend"

{-                    
initCompLogger :: CompM (GenericHandler Handle)
initCompLogger =
    do h <- lift $ fileHandler "debug.log" Log.DEBUG >>= \lh -> return $
                setFormatter lh (simpleLogFormatter "[$loggername : $prio] $msg")
       lift $ Log.updateGlobalLogger logCompileFrontend (Log.addHandler h)
       lift $ Log.updateGlobalLogger logCompileFrontend (Log.setLevel Log.DEBUG)
       lift $ Log.updateGlobalLogger logCompileMidend (Log.addHandler h)
       lift $ Log.updateGlobalLogger logCompileMidend (Log.setLevel Log.DEBUG)
       lift $ Log.updateGlobalLogger logCompileBackend (Log.addHandler h)
       lift $ Log.updateGlobalLogger logCompileBackend (Log.setLevel Log.DEBUG)
       lift $ Log.updateGlobalLogger "Low.MemLow" (Log.addHandler h)
       lift $ Log.updateGlobalLogger "Low.MemLow" (Log.setLevel Log.DEBUG)
       lift $ Log.updateGlobalLogger "Low.ProjLow" (Log.addHandler h)
       lift $ Log.updateGlobalLogger "Low.ProjLow" (Log.setLevel Log.DEBUG)
       lift $ Log.updateGlobalLogger "Low.LintLow" (Log.addHandler h)
       lift $ Log.updateGlobalLogger "Low.LintLow" (Log.setLevel Log.DEBUG)
       return h

closeCompLogger :: GenericHandler Handle -> CompM ()
closeCompLogger s =
    do lift $ Log.updateGlobalLogger logCompileFrontend (Log.removeHandler)
       lift $ Log.updateGlobalLogger logCompileMidend (Log.removeHandler)
       lift $ Log.updateGlobalLogger logCompileBackend (Log.removeHandler)
       lift $ Log.updateGlobalLogger "Low.MemLow" (Log.removeHandler)
       lift $ Log.updateGlobalLogger "Low.ProjLow" (Log.removeHandler)
       lift $ Log.updateGlobalLogger "Low.LintLow" (Log.removeHandler)
       lift $ close s
-}          
    
debugM :: String -> String -> CompM ()
debugM logger msg =
    -- lift $ Log.debugM logger msg
    traceM $ logger ++ " " ++ msg

                    
compErrMod :: String -> String -> a
compErrMod moduleName msg = error $ "[" ++ moduleName ++ "] | " ++ msg
           
           
-----------------------------------
-- == Compiler mode

data GPUStrat = BlkStrat
              | RedStrat
                deriving (Show)

data Target = CPU
            | GPU GPUStrat
            | Device
              deriving (Show)
