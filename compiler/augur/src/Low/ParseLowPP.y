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
  
module Low.ParseLowPP
  ( parse
  , runParseProg
  , runParseStmt ) where

import AstUtil.Pretty
import AstUtil.Var
import Comm.DistSyn
import Comm.Prim
import Low.LowSyn
import Core.CoreTySyn
import Low.LowpPrimSyn
import Low.LexLowPP

import Debug.Trace
import Data.Char
import Control.Monad.Except
}


%name parse prog
%name parseStmt body
%tokentype { LowpResult }
%error { parseError }


%monad { P }
%lexer { lexer } { L _ LowpT_EOF _ }


%token
  '(' { L _ LowpT_LParen _ }
  ')' { L _ LowpT_RParen _ }
  '[' { L _ LowpT_LBrack _ }
  ']' { L _ LowpT_RBrack _ }
  '{' { L _ LowpT_LBrace _ }
  '}' { L _ LowpT_RBrace _ }
  ';' { L _ LowpT_Semi _ }
  ':' { L _ LowpT_Colon _ }
  ',' { L _ LowpT_Comma _ }
  '.' { L _ LowpT_Dot _ }

  '<-' { L _ LowpT_LArr _ }
  ':=' { L _ LowpT_CEq _ }
  '=' { L _ LowpT_Eq _ }
  '+' { L _ LowpT_Plus _ }
  '-' { L _ LowpT_Minus _ }
  '*' { L _ LowpT_Times _ }
  '/' { L _ LowpT_Divide _ }

  'model' { L _ LowpT_Model _ }
  'data' { L _ LowpT_ModData _ }
  'param' { L _ LowpT_ModParam _ }
  'aux' { L _ LowpT_ModAux _ }
  'def' { L _ LowpT_Def _ }
  'seq' { L _ LowpT_Seq _ }
  'par' { L _ LowpT_Par _ }
  'atmpar' { L _ LowpT_AtmPar _ }
  'loop' { L _ LowpT_Loop _ }
  'mapred' { L _ LowpT_MapRed _ }
  'until' { L _ LowpT_Until _ }
  'if' { L _ LowpT_If _ }
  'return' { L _ LowpT_Return _ }
  'Unit' { L _ LowpT_UnitTy _ }
  'Int' { L _ LowpT_IntTy _ }
  'Real' { L _ LowpT_RealTy _ }
  'Vec' { L _ LowpT_VecTy _ }
  'Mat' { L _ LowpT_MatTy _ }

  DIST { L _ (LowpT_Dist $$) _ }
  'sample' { L _ LowpT_Sample _ }

  VAR { L _ (LowpT_VarId $$) _ }
  INT { L _ (LowpT_Int $$) _ }
  FLT { L _ (LowpT_Flt $$) _ }


%left '+' '-'
%left '*' '/'


%%

prog :: { ([(IdKind, String, Typ)], Prog String) }
prog : model decls { ($1, Prog $2) }

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

decls :: { [Decl String] }
decls : {- Empty -} { [] }
      | decl decls { $1 : $2 }

decl :: { Decl String }
decl : 'def' VAR '(' params ')' ':' typ '{' body retstmt '}' { Fun (mkName $2) $4 [] $9 $10 $7 }

params :: { [(String, Typ)] }
params : param ',' params { $1 : $3 }
       | param { [$1] }
       | {- Empty -} { [] }

param :: { (String, Typ) }
param : VAR ':' typ { ($1, $3) }

retstmt :: { Maybe (Exp String) }
retstmt : 'return' exp ';' { Just $2 }
        | {- Empty -} { Nothing }

body :: { Stmt String }
body : stmt body { Seq $1 $2 }
     | {- Empty -} { Skip }
     

stmt :: { Stmt String }
stmt : loopkind 'loop' '(' VAR '<-' gen ')' '{' body '}' { Loop $1 $4 $6 $9 }
     | VAR '[' exps ']' ':=' exp ';' { Store $1 $3 Update $6 }
     | VAR ':=' exp ';' { Store $1 [] Update $3 }
     | VAR '=' exp ';' { Assign $1 $3 }
     | exp ';' { Exp $1 } 

loopkind :: { LoopKind }
loopkind : 'atmpar' { AtomicPar }
         | 'par' { Parallel }
         | 'seq' { Sequential }

gen :: { Gen String }
gen : exp 'until' exp { Until $1 $3 }

exps :: { [Exp String] }
exps : exp ',' exps { $1 : $3 }
     | exp { [$1] }
     | {- empty -} { [] }

exp :: { Exp String }
exp : exp '*' exp { Call (PrimId DM_Fn PM_Fn Times) [ $1, $3 ] }
    | exp '/' exp { Call (PrimId DM_Fn PM_Fn Div) [ $1, $3 ] }
    | exp '+' exp { Call (PrimId DM_Fn PM_Fn Plus) [ $1, $3 ] }
    | exp '-' exp { Call (PrimId DM_Fn PM_Fn Minus) [ $1, $3 ] }
    | cexp { $1 } 

cexp :: { Exp String }
cexp : DIST '(' exps ')' { DistOp (fst $1) DM_Fn (snd $1) $3 }
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
  
parseError :: LowpResult -> P a
parseError (L pos tok stuff) = throwP $ "parse error" ++ show pos ++ show tok ++ show stuff

runParseStmt :: String -> Either String (Stmt String)
runParseStmt str = evalP str parseStmt

runParseProg :: String -> Either String ([(IdKind, String, Typ)], Prog String)
runParseProg str = evalP str parse
  
}
