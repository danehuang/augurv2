{
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

module Rv.LexRv
  ( RvResult(..)
  , RvToken(..)
  , P, throwP, evalP
  , scanner
  , lexer
  , lexwrap ) where

import Comm.DistSyn
import Low.LowpPrimSyn

import Control.Monad.State
import Control.Monad.Except

}


%wrapper "monad"

$whitechar = [\t\n\r\f\v]
$special   = [\(\)\,\;\[\]\{\}]
$digit     = 0-9
$alphalow  = [ a-z ]
$alphaup   = [ A-Z ]
$alpha     = [ $alphalow $alphaup ]
$alphanum  = [ $alpha $digit ]
$ascsymbol = [\+\-\*\/\=]
$newline   = [\n\r]


@dist = Bernoulli | Categorical | Geometric | Poisson
      | Beta | Exponential | Gamma | InvGamma | Normal | Uniform
      | Dirichlet | MvNormal | InvWishart

@prim = sigmoid | dotprod

@decimal   = $digit+

@varid = $alpha [$alphanum \_]*

tokens :-
<0> $white+               { skip }


-- Operators
<0> "<-"                  { mkL RvT_LArr }
<0> "=>"                  { mkL RvT_RArr }
<0> "(*)"                 { mkL RvT_OTimes }

-- Special
<0> \(                    { mkL RvT_LParen }
<0> \)                    { mkL RvT_RParen }
<0> \[                    { mkL RvT_LBrack }
<0> \]                    { mkL RvT_RBrack }
<0> \{                    { mkL RvT_LBrace }
<0> \}                    { mkL RvT_RBrace }
<0> \;                    { mkL RvT_Semi }
<0> \:                    { mkL RvT_Colon }
<0> \,                    { mkL RvT_Comma }
<0> \.                    { mkL RvT_Dot }
<0> \|                    { mkL RvT_Bar }
<0> \~                    { mkL RvT_Tilde }

<0> \=                    { mkL RvT_Eq }
<0> \+                    { mkL RvT_Plus }
<0> \-                    { mkL RvT_Minus }
<0> \*                    { mkL RvT_Times }
<0> \/                    { mkL RvT_Divide }

-- Keywords
<0> "data"                { mkL RvT_ModData }
<0> "param"               { mkL RvT_ModParam }
<0> "Pi"                  { mkL RvT_Pi }
<0> "let"                 { mkL RvT_Let }
<0> "in"                  { mkL RvT_In }
<0> "for"                 { mkL RvT_For }
<0> "until"               { mkL RvT_Until }
<0> "Unit"                { mkL RvT_UnitTy }
<0> "Int"                 { mkL RvT_IntTy }
<0> "Real"                { mkL RvT_RealTy }
<0> "Vec"                 { mkL RvT_VecTy }
<0> "Mat"                 { mkL RvT_MatTy }

<0> "MWG"                 { mkL RvT_MWG }
<0> "HMC"                 { mkL RvT_HMC }
<0> "NUTS"                { mkL RvT_NUTS }
<0> "DiscGibbs"           { mkL RvT_DiscGibbs }
<0> "ConjGibbs"           { mkL RvT_ConjGibbs }
<0> "ESlice"              { mkL RvT_ESlice }
<0> "RSlice"              { mkL RvT_RSlice }

-- Distributions
<0> @dist                 { mkDist Pdf  }

-- Built-in functions
<0> @prim                 { mkPrim }

<0> @varid                { mkVarId }
<0> $digit+ \. $digit*    { mkFlt }
<0> $digit+               { mkInt }


{
data RvResult = L AlexPosn RvToken (Maybe String)
     deriving (Show)

data RvToken
  = RvT_LParen
  | RvT_RParen
  | RvT_LBrack
  | RvT_RBrack
  | RvT_LBrace
  | RvT_RBrace
  | RvT_Semi
  | RvT_Colon
  | RvT_Comma
  | RvT_Dot
  | RvT_Bar
  | RvT_Tilde

  | RvT_OTimes
  | RvT_LArr
  | RvT_RArr
  | RvT_Eq
  | RvT_Plus
  | RvT_Minus
  | RvT_Times
  | RvT_Divide

  | RvT_For
  | RvT_Until
  | RvT_ModData
  | RvT_ModParam
  | RvT_Pi
  | RvT_Let
  | RvT_In
  | RvT_UnitTy
  | RvT_IntTy
  | RvT_RealTy
  | RvT_VecTy
  | RvT_MatTy

  | RvT_MWG
  | RvT_HMC
  | RvT_NUTS
  | RvT_DiscGibbs
  | RvT_ConjGibbs
  | RvT_ESlice
  | RvT_RSlice

  | RvT_Dist (Dop, Dist)
  | RvT_Prim Prim

  | RvT_VarId String
  | RvT_Int Int
  | RvT_Flt Double

  | RvT_EOF
  deriving (Eq, Show)


-- Our parser monad
type P = Alex

evalP :: String -> P a -> Either String a
evalP = runAlex

throwP :: String -> P a
throwP = alexError

mkL :: RvToken -> AlexInput -> Int -> P RvResult
mkL tok (pos, _, _, str) len = return (L pos tok (Just (take len str)))

mkVarId :: AlexInput -> Int -> P RvResult
mkVarId (pos, _, _, str) len = return (L pos (RvT_VarId str') (Just str'))
    where str' = take len str

mkFlt :: AlexInput -> Int -> P RvResult
mkFlt (pos, _, _, str) len = return (L pos (RvT_Flt (read str')) (Just str'))
    where str' = take len str

mkInt :: AlexInput -> Int -> P RvResult
mkInt (pos, _, _, str) len = return (L pos (RvT_Int (read str')) (Just str'))
    where str' = take len str

mkDist :: Dop -> AlexInput -> Int -> P RvResult
mkDist dop (pos, _, _, str) len = return (L pos (RvT_Dist (dop, (foo str'))) (Just str'))
    where
      str' = take len str

      foo "Bernoulli" = Bernoulli
      foo "Categorical" = Categorical
      foo "Geometric" = Geometric
      foo "Poisson" = Poisson
      foo "Beta" = Beta
      foo "Exponential" = Exponential
      foo "Gamma" = Gamma
      foo "InvGamma" = InvGamma
      foo "Normal" = Normal
      foo "Uniform" = Uniform
      foo "Dirichlet" = Dirichlet
      foo "MvNormal" = MvNormal
      foo "InvWishart" = InvWishart
      foo str = error $ " HERE: " ++ str

mkPrim :: AlexInput -> Int -> P RvResult
mkPrim (pos, _, _, str) len = return (L pos (RvT_Prim (foo str')) (Just str'))
    where
      str' = take len str
      
      foo "sigmoid" = Expit
      foo "dotprod" = DotProd

alexEOF :: P RvResult
alexEOF = return (L undefined RvT_EOF Nothing)

scanner :: String -> Either String [RvResult]
scanner str = runAlex str loop
    where
      loop = do res <- alexMonadScan
                case res of
		  L pos RvT_EOF _ -> return [res]
		  L pos _ _ ->
		    do ress <- loop
		       return (res:ress)

lexer :: (RvResult -> P a) -> P a
lexer k =
    do t <- alexMonadScan
       k t

lexwrap :: (RvResult -> P RvResult) -> P RvResult
lexwrap = lexer

}
