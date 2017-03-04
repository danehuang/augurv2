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
  
module Core.ParseCore
  ( parse
  , runParseProg
) where

import AstUtil.Pretty
import AstUtil.Var
import Comm.DistSyn
import Core.CoreTySyn
import Low.LowpPrimSyn
import Core.DensSyn
import qualified Core.KernSyn as K
import Core.LexCore

import Debug.Trace
import Data.Char
import Control.Monad.Except
}


%name parse prog
%tokentype { DensResult }
%error { parseError }


%monad { P }
%lexer { lexer } { L _ DensT_EOF _ }


%token
  '(' { L _ DensT_LParen _ }
  ')' { L _ DensT_RParen _ }
  '[' { L _ DensT_LBrack _ }
  ']' { L _ DensT_RBrack _ }
  '{' { L _ DensT_LBrace _ }
  '}' { L _ DensT_RBrace _ }
  ';' { L _ DensT_Semi _ }
  ':' { L _ DensT_Colon _ }
  ',' { L _ DensT_Comma _ }
  '.' { L _ DensT_Dot _ }
  '|' { L _ DensT_Bar _ }

  '<-' { L _ DensT_LArr _ }
  '=' { L _ DensT_Eq _ }
  '+' { L _ DensT_Plus _ }
  '-' { L _ DensT_Minus _ }
  '*' { L _ DensT_Times _ }
  '/' { L _ DensT_Divide _ }

  'model' { L _ DensT_Model _ }
  'data' { L _ DensT_ModData _ }
  'param' { L _ DensT_ModParam _ }
  'aux' { L _ DensT_ModAux _ }
  'Pi' { L _ DensT_Pi _ }
  'let' { L _ DensT_Let _ }
  in { L _ DensT_In _ }
  'until' { L _ DensT_Until _ }
  'Unit' { L _ DensT_UnitTy _ }
  'Int' { L _ DensT_IntTy _ }
  'Real' { L _ DensT_RealTy _ }
  'Vec' { L _ DensT_VecTy _ }
  'Mat' { L _ DensT_MatTy _ }

  'HMC' { L _ DensT_HMC _ }
  'DiscGibbs' { L _ DensT_DiscGibbs _ }
  'ConjGibbs' { L _ DensT_ConjGibbs _ }
  'Slice' { L _ DensT_Slice _ }

  DIST { L _ (DensT_Dist $$) _ }

  VAR { L _ (DensT_VarId $$) _ }
  INT { L _ (DensT_Int $$) _ }
  FLT { L _ (DensT_Flt $$) _ }


%right in
%left '+' '-'
%left '*' '/'


%%

prog :: { ([(IdKind, String, Typ)], K.KernU String) }
prog : model ker { ($1, $2) }

model :: { [(IdKind, String, Typ)] }
model : {- Empty -} { [] }
      | 'model' '{' moddecls '}' { $3 }

moddecls :: { [(IdKind, String, Typ)] }
moddecls : {- Empty -} { [] }
         | moddecl moddecls { $1 : $2 }

moddecl :: { (IdKind, String, Typ) }
moddecl : 'aux' VAR ':' typ ';' { (ModAux, $2, $4) }
        | 'param' VAR ':' typ ';' { (ModParam PK_Prob, $2, $4) } -- TODO: HACK
        | 'data' VAR ':' typ ';' { (ModData, $2, $4) }

vars :: { [String] }
vars : VAR { [$1] }
     | VAR ',' vars { $1 : $3 }

ker :: { K.KernU String }
ker : atmker ker { K.Tensor $1 $2 }
    | atmker { $1 }

atmker :: { K.KernU String }
atmker :
-- 'HMC' '[' vars ']' '{' body '}' { K.Base (K.GradProp (K.HMC () ())) (K.Block $3) $6 [] () }
'HMC' '[' vars ']' hmcsettings { K.mkUserHMC $3 $5 }
-- | 'DiscGibbs' '[' VAR ']' '{' body '}' { K.Base (K.Gibbs (K.Disc ())) (K.Single $3) $6 [] () }
| 'DiscGibbs' '[' VAR ']' { K.mkUserDiscGibbs $3 }
-- | 'ConjGibbs' '[' VAR ']' '{' body '}' { K.Base (K.Gibbs (K.Conj () ())) (K.Single $3) $6 [] () }
| 'ConjGibbs' '[' VAR ']' { K.mkUserConjGibbs $3 }
-- | 'Slice' '[' VAR ']' '{' body '}' { K.Base (K.Slice (K.Ellip () ())) (K.Single $3) $6 [] () }
| 'Slice' '[' VAR ']' { K.mkUserESlice $3 }

hmcsettings :: { Maybe (Double, Double) }
hmcsettings : {- Empty -} { Nothing }
            | '[' FLT ',' FLT ']' { Just ($2, $4) }

body :: { Fn String }
body : fn body { Prod $1 $2 }
     | fn { $1 }
     
fn :: { Fn String }
fn : DIST '(' exp ';' exps  ')' { Dens (snd $1) $3 $5 }
   | '[' body  '|' indconds ']' { Ind $2 $4 }
   | 'let' VAR '=' exp in fn { Let $2 $4 $6 }
   | 'Pi' '(' VAR '<-' gen ')' '{' body '}' { Pi $3 $5 $8 }
   | '(' body ')' { $2 }

denspt :: { DensPt String }
denspt : VAR '[' vars ']' { DensPt ($1, $3) }
       | VAR { DensPt ($1, []) }

indconds :: { [IndCond String] }
indconds : indcond { [ $1 ] }
         | indcond ',' indconds { $1 : $3 }

indcond :: { IndCond String }
indcond : VAR '=' exp { CatCond $1 $3 }

gen :: { Gen String }
gen : exp 'until' exp { Until $1 $3 }

exps :: { [Exp String] }
exps : exp ',' exps { $1 : $3 }
     | exp { [$1] }
     | {- empty -} { [] }

exp :: { Exp String }
exp : exp '*' exp { Call (PrimId Times) [ $1, $3 ] }
    | exp '/' exp { Call (PrimId Div) [ $1, $3 ] }
    | exp '+' exp { Call (PrimId Plus) [ $1, $3 ] }
    | exp '-' exp { Call (PrimId Minus) [ $1, $3 ] }
    | cexp { $1 } 

cexp :: { Exp String }
cexp : DIST '(' exps ')' { DistOp (fst $1) (snd $1) $3 }
     | aexp { $1 }

aexp :: { Exp String }
aexp : INT { Lit (Int $1) }
     | FLT { Lit (Real $1) }
     | VAR { Var $1 }
     | VAR '[' exps ']' { Proj (Var $1) $3 }
     | '(' exp ')' { $2 }

typ :: { Typ }
typ : 'Unit' { UnitTy }
| 'Int' { IntTy }
| 'Real' { RealTy }
| 'Vec' typ { VecTy $2 }
| 'Mat' typ { MatTy $2 }
| '(' typ ')' { $2 }

 
{
  
parseError :: DensResult -> P a
parseError (L pos tok stuff) = throwP $ "parse error" ++ show pos ++ show tok ++ show stuff

runParseProg :: String -> Either String ([(IdKind, String, Typ)], K.KernU String)
runParseProg str = evalP str parse
  
}
