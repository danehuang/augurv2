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
  
module Rv.ParseRv
  ( parse
  , runParseModel
  , runParseKer
) where

import AstUtil.Pretty
import AstUtil.Var
import Comm.DistSyn
import Core.CoreTySyn
import Low.LowpPrimSyn
import Core.DensSyn
import Rv.RvSyn
import qualified Core.KernSyn as K
import Rv.LexRv

import Debug.Trace
import Data.Char
import Control.Monad.Except
}


%name parse model
%tokentype { RvResult }
%error { parseError }

%name parseKer ker

%monad { P }
%lexer { lexer } { L _ RvT_EOF _ }


%token
  '(' { L _ RvT_LParen _ }
  ')' { L _ RvT_RParen _ }
  '[' { L _ RvT_LBrack _ }
  ']' { L _ RvT_RBrack _ }
  '{' { L _ RvT_LBrace _ }
  '}' { L _ RvT_RBrace _ }
  ';' { L _ RvT_Semi _ }
  ':' { L _ RvT_Colon _ }
  ',' { L _ RvT_Comma _ }
  '.' { L _ RvT_Dot _ }
  '|' { L _ RvT_Bar _ }
  '~' { L _ RvT_Tilde _ }

  '(*)' { L _ RvT_OTimes _ }
  '<-' { L _ RvT_LArr _ }
  '=>' { L _ RvT_RArr _ }
  '=' { L _ RvT_Eq _ }
  '+' { L _ RvT_Plus _ }
  '-' { L _ RvT_Minus _ }
  '*' { L _ RvT_Times _ }
  '/' { L _ RvT_Divide _ }

  'data' { L _ RvT_ModData _ }
  'param' { L _ RvT_ModParam _ }
  'let' { L _ RvT_Let _ }
  'in' { L _ RvT_In _ }
  'for' { L _ RvT_For _ }
  'until' { L _ RvT_Until _ }
  'Unit' { L _ RvT_UnitTy _ }
  'Int' { L _ RvT_IntTy _ }
  'Real' { L _ RvT_RealTy _ }
  'Vec' { L _ RvT_VecTy _ }
  'Mat' { L _ RvT_MatTy _ }

  'MWG' { L _ RvT_MWG _ }
  'HMC' { L _ RvT_HMC _ }
  'DiscGibbs' { L _ RvT_DiscGibbs _ }
  'ConjGibbs' { L _ RvT_ConjGibbs _ }
  'ESlice' { L _ RvT_ESlice _ }

  DIST { L _ (RvT_Dist $$) _ }
  PRIM { L _ (RvT_Prim $$) _ }

  VAR { L _ (RvT_VarId $$) _ }
  INT { L _ (RvT_Int $$) _ }
  FLT { L _ (RvT_Flt $$) _ }


%left '='
%left '~'
%right in
%left '+' '-'
%left '*' '/'


%%

model :: { Model } 
model : '(' params ')' '=>' '{' decls '}' { Model $2 $6 }

params :: { [(IdKind, String, Typ)] }
params : {- Empty -} { [] }
       | param { [$1] }
       | param ',' params { $1 : $3 }

param :: { (IdKind, String, Typ) }
param : VAR ':' typ { (ModHyper, $1, $3) }

ker :: { K.KernU String }
ker : atmker '(*)' ker { K.Tensor $1 $3 }
    | atmker { $1 }

atmker :: { K.KernU String }
atmker :
-- 'HMC' '[' vars ']' { K.Base (K.GradProp (K.HMC () ())) (K.Block $3) (prodFn (map (\v -> dirac v []) $3)) [] () }
'HMC' '[' vars ']' hmcsettings { K.mkUserHMC $3 $5 }
| 'MWG' '[' VAR ']' '~' exp propfoo { K.mkUserMWG $3 $6 $7 }
-- | 'DiscGibbs' '[' VAR ']' { K.Base (K.Gibbs (K.Disc ())) (K.Single $3) (dirac $3 []) [] () }
| 'DiscGibbs' '[' VAR ']' { K.mkUserDiscGibbs $3 }
-- | 'ConjGibbs' '[' VAR ']' { K.Base (K.Gibbs (K.Conj () ())) (K.Single $3) (dirac $3 []) [] () }
| 'ConjGibbs' '[' VAR ']' { K.mkUserConjGibbs $3 }
-- | 'ESlice' '[' VAR ']' { K.Base (K.Slice (K.Ellip () ())) (K.Single $3) (dirac $3 []) [] ()}
| 'ESlice' '[' VAR ']' { K.mkUserESlice $3 }


hmcsettings :: { Maybe (Double, Double) }
hmcsettings : {- Empty -} { Nothing }
            | '[' FLT ',' FLT ']' { Just ($2, $4) }

propfoo :: { [(String, Gen String)] }
propfoo : {- Empty -} { [] }
        | 'for' grids { $2 }


decls :: { [Decl] }
decls : {- Empty -} { [] }
      | decl decls { $1 : $2 }

-- mkDecl :: IdKind -> String -> [String] -> Typ -> Bool -> Exp String -> [(String, Gen String)]

decl :: { Decl }
decl : idkind VAR asskind exp ';' { mkDecl $1 $2 [] UnitTy $3 $4 [] }
     -- idkind VAR asskind exp ';' { if $3 then Decl $2 UnitTy $1 [] $4 else Decl $2 UnitTy $1 [] (DistOp Sample Dirac [$4]) }
     | idkind VAR '[' vars ']' asskind exp 'for' grids ';' { mkDecl $1 $2 $4 UnitTy $6 $7 $9 }
     -- | idkind VAR '[' vars ']' asskind exp 'for' grids ';' { if $6 then Decl $2 UnitTy $1 (reverse $9) $7 else Decl $2 UnitTy $1 (reverse $9) (DistOp Sample Dirac [$7]) }
     | idkind VAR '[' vars ']' ':' typ asskind exp 'for' grids ';' { mkDecl $1 $2 $4 $7 $8 $9 $11 }
     -- | idkind VAR '[' vars ']' ':' typ asskind exp 'for' grids ';' { if $8 then Decl $2 $7 $1 (reverse $11) $9 else Decl $2 $7 $1 (reverse $11) (DistOp Sample Dirac [$9]) }
     -- | 'param' VAR '[' vars ']' ':' exp 'for' grids ';' { Decl $2 UnitTy ModParam (reverse $9) (DistOp Sample Dirac [$7]) }

asskind :: { Bool }
asskind : '~' { True }
        | '=' { False }

grids :: { [(String, Gen String)] }
grids : grid                           { [$1] }
      | grids ',' grid                 { $3 : $1 }

grid :: { (String, Gen String) }
grid : VAR '<-' gen                    { ($1, $3) }
	      
vars :: { [String] }
vars : VAR { [$1] }
     | VAR ',' vars { $1 : $3 }

idkind :: { IdKind }
idkind : 'param' { ModParam PK_Prob }
       | 'data' { ModData }

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
     | PRIM '(' exps ')' { Call (PrimId $1) $3 }
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

mkDecl :: IdKind -> String -> [String] -> Typ -> Bool -> Exp String -> [(String, Gen String)] -> Decl
mkDecl ik v_mod idxs ty asskind e grid =
    if asskind
    then Decl v_mod ty (f ik asskind) (reverse grid) e
    else Decl v_mod ty (f ik asskind) (reverse grid) (DistOp Sample Dirac [e])
  where
    f (ModParam _) True = ModParam PK_Prob
    f (ModParam _) False = ModParam PK_Det
    f ModData _ = ModData
  
parseError :: RvResult -> P a
parseError (L pos tok stuff) = throwP $ "parse error" ++ show pos ++ show tok ++ show stuff

runParseModel :: String -> Either String Model
runParseModel str = evalP str parse

runParseKer :: String -> Either String (K.KernU String)
runParseKer str = evalP str parseKer
  
}
