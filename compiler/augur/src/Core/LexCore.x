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

module Core.LexCore
  ( DensResult(..)
  , DensToken(..)
  , P, throwP, evalP
  , scanner
  , lexer
  , lexwrap ) where

import Comm.DistSyn

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

@decimal   = $digit+

@varid = $alpha [$alphanum \_]*

tokens :-
<0> $white+               { skip }


-- Special
<0> \(                    { mkL DensT_LParen }
<0> \)                    { mkL DensT_RParen }
<0> \[                    { mkL DensT_LBrack }
<0> \]                    { mkL DensT_RBrack }
<0> \{                    { mkL DensT_LBrace }
<0> \}                    { mkL DensT_RBrace }
<0> \;                    { mkL DensT_Semi }
<0> \:                    { mkL DensT_Colon }
<0> \,                    { mkL DensT_Comma }
<0> \.                    { mkL DensT_Dot }
<0> \|                    { mkL DensT_Bar }

<0> \=                    { mkL DensT_Eq }
<0> \+                    { mkL DensT_Plus }
<0> \-                    { mkL DensT_Minus }
<0> \*                    { mkL DensT_Times }
<0> \/                    { mkL DensT_Divide }


-- Operators
<0> "<-"                  { mkL DensT_LArr }

-- Keywords
<0> "model"               { mkL DensT_Model }
<0> "aux"                 { mkL DensT_ModAux }
<0> "data"                { mkL DensT_ModData }
<0> "param"               { mkL DensT_ModParam }
<0> "Pi"                  { mkL DensT_Pi }
<0> "let"                 { mkL DensT_Let }
<0> "in"                  { mkL DensT_In }
<0> "until"               { mkL DensT_Until }
<0> "Unit"                { mkL DensT_UnitTy }
<0> "Int"                 { mkL DensT_IntTy }
<0> "Real"                { mkL DensT_RealTy }
<0> "Vec"                 { mkL DensT_VecTy }
<0> "Mat"                 { mkL DensT_MatTy }

<0> "HMC"                 { mkL DensT_HMC }
<0> "DiscGibbs"           { mkL DensT_DiscGibbs }
<0> "ConjGibbs"           { mkL DensT_ConjGibbs }
<0> "Slice"               { mkL DensT_Slice }

-- Distributions
<0> @dist                 { mkDist Pdf  }

<0> @varid                { mkVarId }
<0> $digit+ \. $digit*    { mkFlt }
<0> $digit+               { mkInt }


{
data DensResult = L AlexPosn DensToken (Maybe String)
     deriving (Show)

data DensToken
  = DensT_LParen
  | DensT_RParen
  | DensT_LBrack
  | DensT_RBrack
  | DensT_LBrace
  | DensT_RBrace
  | DensT_Semi
  | DensT_Colon
  | DensT_Comma
  | DensT_Dot
  | DensT_Bar
  
  | DensT_LArr
  | DensT_Eq
  | DensT_Plus
  | DensT_Minus
  | DensT_Times
  | DensT_Divide

  | DensT_Until
  | DensT_Model
  | DensT_ModData
  | DensT_ModParam
  | DensT_Pi
  | DensT_Let
  | DensT_In
  | DensT_ModAux
  | DensT_UnitTy
  | DensT_IntTy
  | DensT_RealTy
  | DensT_VecTy
  | DensT_MatTy

  | DensT_HMC
  | DensT_DiscGibbs
  | DensT_ConjGibbs
  | DensT_Slice

  | DensT_Dist (Dop, Dist)

  | DensT_VarId String
  | DensT_Int Int
  | DensT_Flt Double

  | DensT_EOF
  deriving (Eq, Show)


-- Our parser monad
type P = Alex

evalP :: String -> P a -> Either String a
evalP = runAlex

throwP :: String -> P a
throwP = alexError

mkL :: DensToken -> AlexInput -> Int -> P DensResult
mkL tok (pos, _, _, str) len = return (L pos tok (Just (take len str)))

mkVarId :: AlexInput -> Int -> P DensResult
mkVarId (pos, _, _, str) len = return (L pos (DensT_VarId str') (Just str'))
    where str' = take len str

mkFlt :: AlexInput -> Int -> P DensResult
mkFlt (pos, _, _, str) len = return (L pos (DensT_Flt (read str')) (Just str'))
    where str' = take len str

mkInt :: AlexInput -> Int -> P DensResult
mkInt (pos, _, _, str) len = return (L pos (DensT_Int (read str')) (Just str'))
    where str' = take len str

mkDist :: Dop -> AlexInput -> Int -> P DensResult
mkDist dop (pos, _, _, str) len = return (L pos (DensT_Dist (dop, (foo str'))) (Just str'))
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

alexEOF :: P DensResult
alexEOF = return (L undefined DensT_EOF Nothing)

scanner :: String -> Either String [DensResult]
scanner str = runAlex str loop
    where
      loop = do res <- alexMonadScan
                case res of
		  L pos DensT_EOF _ -> return [res]
		  L pos _ _ ->
		    do ress <- loop
		       return (res:ress)

lexer :: (DensResult -> P a) -> P a
lexer k =
    do t <- alexMonadScan
       k t

lexwrap :: (DensResult -> P DensResult) -> P DensResult
lexwrap = lexer

}


