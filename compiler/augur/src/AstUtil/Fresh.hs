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

module AstUtil.Fresh where

import Data.IORef

----------------------------------------------------------------------
-- = Fresh Description
{-| [Note]

Generate basic fresh variables. 

-}


-----------------------------------
-- == Types and operations

type GenSym = IORef Int            
    
newGenSym :: IO GenSym
newGenSym = newIORef 0
    
freshSym :: GenSym -> IO Int
freshSym genSym =
    do sym <- readIORef genSym
       writeIORef genSym (sym + 1)
       return sym
