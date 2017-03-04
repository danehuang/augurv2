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

module Xface.PyXface where

import Data.Maybe
import Control.Monad.Reader
    
import AstUtil.Var
import AstUtil.Pretty
import AstUtil.Fresh
import Core.DensSyn
import Core.CoreSyn
import Comm.DistSyn
import Core.CoreTySyn
import qualified Xface.PySyn as P
import Low.LowpPrimSyn
import Compile.CompData
    

----------------------------------------------------------------------
-- = PyXface description
{-| [Note]

Generates python inferface code.

Order:
  [ hyper_1, ..., hyper_n, param_1, ..., param_m, data_1, ..., data_l ]


-}


-----------------------------------
-- == Generate model object

type XfaceM = ReaderT XfaceRdr CompM
type TIdTy = TVar Typ
    
data XfaceRdr =
    XR { pi_ty :: Typ
       , pi_vInit :: TIdTy
       , pi_genSym :: GenSym }


withTyp :: Typ -> XfaceM a -> XfaceM a
withTyp ty = local (\rdr -> rdr { pi_ty = ty })
    

mkAnonPyId :: Typ -> XfaceM TIdTy
mkAnonPyId ty =
    do genSym <- asks pi_genSym
       v <- lift $ lift $ mkIdIO genSym Anon Local :: XfaceM TIdTy
       return $ v { t_ty = Just ty }

              
cgLit :: Lit -> P.Lit
cgLit (Int i) = P.Int i
cgLit (Real d) = P.Dbl d


cgDist :: Dop -> Dist -> [Exp TIdTy] -> XfaceM (P.Exp TIdTy)
cgDist dop dist es =
    case dop of
      Sample ->
          do vInit <- asks pi_vInit
             es' <- mapM cgExp es
             return $ P.Call ("init" ++ pprShow dist) (P.Var vInit : es')
      _ -> error $ "[PyXface] @cgDist | Shouldn't happen"
                 

cgPrim :: Prim -> [P.Exp TIdTy] -> P.Exp TIdTy
cgPrim prim es' =
    case prim of
      Neg -> error "[PyXface] | TODO neg"
      Expon -> P.Call "augur_exp" es'
      Log -> P.Call "augur_log" es'
      Expit -> P.Call "augur_expit" es'
      Logit -> P.Call "augur_logit" es'
      Plus -> P.Binop (es' !! 0) P.Plus (es' !! 1)
      Minus -> P.Binop (es' !! 0) P.Minus (es' !! 1)
      Times -> P.Binop (es' !! 0) P.Times (es' !! 1)
      Div -> P.Binop (es' !! 0) P.Div (es' !! 1)
      DotProd -> P.Call "augur_dotprod" es'
      EqEq -> P.Binop (es' !! 0) P.EqEq (es' !! 1)      
      LAnd -> P.Binop (es' !! 0) P.LAnd (es' !! 1)
      _ -> error $ "[PyXface] @cgPrim | Shouldn't happen: " ++ pprShow prim
          
           
cgExp :: Exp TIdTy -> XfaceM (P.Exp TIdTy)
cgExp (Var x) = return $ P.Var x
cgExp (Lit lit) = return $ P.Lit (cgLit lit)
cgExp (DistOp dop dist es) = cgDist dop dist es
cgExp (Call ce es) =
    case ce of
      FnId _ -> error $ "[PyXface] @cgExp | Not supported yet"
      PrimId prim ->
          do es' <- mapM cgExp es
             return $cgPrim prim es'
cgExp (Proj e es) =
    do e' <- cgExp e
       es' <- mapM cgExp es
       return $ P.Proj e' es'

                  
cgGen :: Gen TIdTy -> XfaceM (P.Gen TIdTy)
cgGen (Until e1 e2) =
    do e1' <- cgExp e1
       e2' <- cgExp e2
       return $ P.Until e1' e2'

                      
genToSize :: Gen TIdTy -> XfaceM (P.Exp TIdTy)
genToSize (Until e1 e2) =
    do e1' <- cgExp e1
       e2' <- cgExp e2
       return $ P.Binop e2' P.Minus e1'
    

unvecTy :: Typ -> Typ
unvecTy (VecTy ty) = ty
unvecTy ty = error $ "[PyXface] | Shouldn't happen " ++ pprShow ty
     
tyToInt :: Typ -> Int
tyToInt UnitTy = error $ "[PyXface] @tyToInt | Shouldn't happen"
tyToInt IntTy = 0
tyToInt RealTy = 1
tyToInt (VecTy _) = 2
tyToInt (MatTy _) = 3
tyToInt (ArrTy _ _) = error $ "[PyXface] @tyToInt | Shouldn't happen"

             
cgPyModParamK :: Fn TIdTy -> ((TIdTy, P.Stmt TIdTy) -> XfaceM (TIdTy, P.Stmt TIdTy)) -> XfaceM (TIdTy, P.Stmt TIdTy)
cgPyModParamK (Dens dist pt es) k =
    do ty <- asks pi_ty
       vSamp <- mkAnonPyId ty
       erhs <- cgDist Sample dist es
       let s = P.Assign vSamp erhs
       k (vSamp, s)
cgPyModParamK (Let x e fn) k =
    cgPyModParamK fn (\(vTmp, s) ->
                          do e' <- cgExp e
                             k (vTmp, P.Seq (P.Assign x e') s))
cgPyModParamK fn@(Ind _ _) _ =
    error $ "[PyXface] | Shouldn't happen " ++ pprShow fn
cgPyModParamK fn@(Prod _ _) _ =
    error $ "[PyXface] | Shouldn't happen " ++ pprShow fn
cgPyModParamK (Pi x gen fn) k =
    do ty <- asks pi_ty
       let ty' = unvecTy ty
       withTyp ty' (cgPyModParamK fn (k' ty'))
    where
      k' ty (vTmp, s) =
          do vArr <- mkAnonPyId ty
             esize <- genToSize gen
             gen' <- cgGen gen
             let s_arr = P.Assign vArr (P.Call "initArr" [ esize, P.Lit (P.Int (tyToInt ty)) ])
                 s_store = P.Store vArr [P.Var x] (P.Var vTmp)
                 s_loop = P.Loop x gen' (P.Seq s s_store)
             k (vArr, P.Seq s_arr s_loop)
                     
cgPyModParam :: ModDecls TIdTy -> TIdTy -> Fn TIdTy -> XfaceM (P.Stmt TIdTy)
cgPyModParam modDecls vMod fn =
    do (vTmp, s) <- withTyp (fromJust (getType vMod)) (cgPyModParamK fn return)
       return $ P.Seq s (P.Assign vMod (P.Var vTmp))
              
              
cgPyModParams :: ModDecls TIdTy -> [(TIdTy, Fn TIdTy)] -> XfaceM (P.Decl TIdTy)
cgPyModParams modDecls fns =
    do stmts <- mapM (\(vMod, fn) -> cgPyModParam modDecls vMod fn) fns
       vInit <- asks pi_vInit
       let name = mkName "genMod"
           hypers = getModHyperIds modDecls
           ret = P.Return (P.List (map (\v -> P.Tup [P.Lit (P.PyStr (nameToStr (varName v))), P.Var v]) (map fst fns)))
           stmts' = stmts ++ [ ret ]
       return $ P.Fun name (vInit : hypers) (P.seqStmt stmts')

              
runCgPyModParams :: CompInfo -> ModDecls TIdTy -> [(TIdTy, Fn TIdTy)] -> CompM (P.Decl TIdTy)
runCgPyModParams cinfo modDecls fns =
    do let fns' = filter (\(v, _) -> isModParam (idKind v)) fns
       vInit <- lift $ mkTyIdIO (getGenSym cinfo) (mkName "initStrat") Local UnitTy
       v <- runReaderT (cgPyModParams modDecls fns') (XR UnitTy vInit (getGenSym cinfo))
       return v


              
-----------------------------------
-- == Generate types

cgTyp :: Typ -> String
cgTyp IntTy = "PyAugurIntTy()"
cgTyp RealTy = "PyAugurRealTy()"
cgTyp (VecTy ty) = "PyAugurVecTy(" ++ cgTyp ty ++ ")"
cgTyp (MatTy ty) = "PyAugurMatTy(" ++ "2" ++ ", " ++ cgTyp ty ++ ")"
cgTyp ty = error $ "@tyToPyTy | Shouldn't happen: " ++ show ty

           
genPyTyp :: ModDecls (TVar Typ) -> String
genPyTyp modDecls =
    let hypers = getModHyperIds modDecls
        params = getModParamIds modDecls
        datas = getModDataIds modDecls
    in
      "(" ++ f hypers ++ ", " ++ f params ++ ", " ++ f datas ++ ")"
    where
      f xs = "[" ++ rendSepBy commasp (map varToStr xs) ++ "]"
      
      varToStr v = "(" ++ "\'" ++ pprShow v ++ "\' , " ++ cgTyp (fromJust (t_ty v)) ++ ")"
