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

module Low.LexLowPP
  ( LowpResult(..)
  , LowpToken(..)
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
<0> \(                    { mkL LowpT_LParen }
<0> \)                    { mkL LowpT_RParen }
<0> \[                    { mkL LowpT_LBrack }
<0> \]                    { mkL LowpT_RBrack }
<0> \{                    { mkL LowpT_LBrace }
<0> \}                    { mkL LowpT_RBrace }
<0> \;                    { mkL LowpT_Semi }
<0> \:                    { mkL LowpT_Colon }
<0> \,                    { mkL LowpT_Comma }
<0> \.                    { mkL LowpT_Dot }
<0> \=                    { mkL LowpT_Eq }
<0> \+                    { mkL LowpT_Plus }
<0> \-                    { mkL LowpT_Minus }
<0> \*                    { mkL LowpT_Times }
<0> \/                    { mkL LowpT_Divide }


-- Operators
<0> "<-"                  { mkL LowpT_LArr }
<0> ":="                  { mkL LowpT_CEq }

-- Keywords
<0> "model"               { mkL LowpT_Model }
<0> "data"                { mkL LowpT_ModData }
<0> "param"               { mkL LowpT_ModParam }
<0> "aux"                 { mkL LowpT_ModAux }
<0> "def"                 { mkL LowpT_Def }
<0> "seq"                 { mkL LowpT_Seq }
<0> "par"                 { mkL LowpT_Par }
<0> "atmpar"              { mkL LowpT_AtmPar }
<0> "loop"                { mkL LowpT_Loop }
<0> "mapred"              { mkL LowpT_MapRed }
<0> "until"               { mkL LowpT_Until }
<0> "if"                  { mkL LowpT_If }
<0> "return"              { mkL LowpT_Return }
<0> "Unit"                { mkL LowpT_UnitTy }
<0> "Int"                 { mkL LowpT_IntTy }
<0> "Real"                { mkL LowpT_RealTy }
<0> "Vec"                 { mkL LowpT_VecTy }
<0> "Mat"                 { mkL LowpT_MatTy }

-- Distributions
<0> @dist \. "sample" { mkDist Sample  }
-- <0> "Bernoulli"           { mkL (LowpT_Dist Bernoulli) }
-- <0> "Categorical"         { mkL (LowpT_Dist Categorical) }
-- <0> "Geometric"           { mkL (LowpT_Dist Geometric) }
-- <0> "Poisson"             { mkL (LowpT_Dist Poisson) }

-- <0> "Beta"                { mkL (LowpT_Dist Beta) }
-- <0> "Exponential"         { mkL (LowpT_Dist Exponential) }
-- <0> "Gamma"               { mkL (LowpT_Dist Gamma) }
-- <0> "InvGamma"            { mkL (LowpT_Dist InvGamma) }
-- <0> "Normal"              { mkL (LowpT_Dist Normal) }
-- <0> "Uniform"             { mkL (LowpT_Dist Uniform) }

-- <0> "Dirichlet"           { mkL (LowpT_Dist Dirichlet) }
-- <0> "MvNormal"            { mkL (LowpT_Dist MvNormal) }
-- <0> "InvWishart"          { mkL (LowpT_Dist InvWishart) }

<0> "sample"              { mkL LowpT_Sample }

<0> @varid                { mkVarId }
<0> $digit+ \. $digit*    { mkFlt }
<0> $digit+               { mkInt }


{
data LowpResult = L AlexPosn LowpToken (Maybe String)
     deriving (Show)

data LowpToken
  = LowpT_LParen
  | LowpT_RParen
  | LowpT_LBrack
  | LowpT_RBrack
  | LowpT_LBrace
  | LowpT_RBrace
  | LowpT_Semi
  | LowpT_Colon
  | LowpT_Comma
  | LowpT_Dot
  
  | LowpT_LArr
  | LowpT_CEq
  | LowpT_Eq
  | LowpT_Plus
  | LowpT_Minus
  | LowpT_Times
  | LowpT_Divide

  | LowpT_Def
  | LowpT_Seq
  | LowpT_Par
  | LowpT_AtmPar
  | LowpT_Loop
  | LowpT_MapRed
  | LowpT_Until
  | LowpT_If
  | LowpT_Return
  | LowpT_Model
  | LowpT_ModData
  | LowpT_ModParam
  | LowpT_ModAux
  | LowpT_UnitTy
  | LowpT_IntTy
  | LowpT_RealTy
  | LowpT_VecTy
  | LowpT_MatTy

  | LowpT_Dist (Dop, Dist)
  | LowpT_Sample

  | LowpT_VarId String
  | LowpT_Int Int
  | LowpT_Flt Double

  | LowpT_EOF
  deriving (Eq, Show)


-- Our parser monad
type P = Alex

evalP :: String -> P a -> Either String a
evalP = runAlex

throwP :: String -> P a
throwP = alexError

mkL :: LowpToken -> AlexInput -> Int -> P LowpResult
mkL tok (pos, _, _, str) len = return (L pos tok (Just (take len str)))

mkVarId :: AlexInput -> Int -> P LowpResult
mkVarId (pos, _, _, str) len = return (L pos (LowpT_VarId str') (Just str'))
    where str' = take len str

mkFlt :: AlexInput -> Int -> P LowpResult
mkFlt (pos, _, _, str) len = return (L pos (LowpT_Flt (read str')) (Just str'))
    where str' = take len str

mkInt :: AlexInput -> Int -> P LowpResult
mkInt (pos, _, _, str) len = return (L pos (LowpT_Int (read str')) (Just str'))
    where str' = take len str

mkDist :: Dop -> AlexInput -> Int -> P LowpResult
mkDist dop (pos, _, _, str) len = return (L pos (LowpT_Dist (dop, (foo (str' dop)))) (Just (str' dop)))
    where
      str' Sample = take (len - 7) str

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
      foo str = error str

alexEOF :: P LowpResult
alexEOF = return (L undefined LowpT_EOF Nothing)

scanner :: String -> Either String [LowpResult]
scanner str = runAlex str loop
    where
      loop = do res <- alexMonadScan
                case res of
		  L pos LowpT_EOF _ -> return [res]
		  L pos _ _ ->
		    do ress <- loop
		       return (res:ress)

lexer :: (LowpResult -> P a) -> P a
lexer k =
    do t <- alexMonadScan
       k t

lexwrap :: (LowpResult -> P LowpResult) -> P LowpResult
lexwrap = lexer

}


