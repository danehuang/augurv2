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


module CudaC.CgCudaCCore where

import qualified Data.Map as Map
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Var
import Core.CoreSyn
import Comm.Prim
import Compile.CompData
import CudaC.XfaceCudaC hiding (compErr)
import qualified CudaC.CudaCSyn as C
import Low.LowSyn
import Core.CoreTySyn
import Low.LowpPrimSyn
    
----------------------------------------------------------------------
-- = CgCudaCCore Description
{-| [Note]

-}

compErr :: String -> a
compErr msg = compErrMod "CgCudaCCore" msg

              
-----------------------------------
-- == Types and operations

tyOf :: Exp (TVar Typ) -> Typ
tyOf (Var x) = getType' x
tyOf (Lit lit) =
    case lit of
      Int _ -> IntTy
      Real _ -> RealTy
tyOf e = compErr $ "Expected atomic expression but found " ++ pprShow e


cgTyp :: IdKind -> Typ -> C.Typ
cgTyp ik ty =
    case ty of
      UnitTy -> C.VoidTy
      IntTy -> if isIndirect ik then C.PtrTy C.IntTy else C.IntTy
      RealTy -> if isIndirect ik then C.PtrTy C.DblTy else C.DblTy
      VecTy _ -> C.PtrTy ty_AugurVec
      MatTy _ -> C.PtrTy ty_AugurMat
      BlkTy _ -> C.PtrTy ty_AugurBlk
      ArrTy _ _ -> compErr $ "Cannot code-gen arrow type."

                                                     
cgTypTop :: Typ -> C.Typ
cgTypTop UnitTy = C.VoidTy
cgTypTop IntTy = C.IntTy
cgTypTop RealTy = C.DblTy
cgTypTop (VecTy _) = ty_AugurFlatVec
cgTypTop (MatTy _) = ty_AugurMat
cgTypTop (BlkTy _) = ty_AugurBlk
cgTypTop (ArrTy _ _) = compErr $ "Cannot code-gen arrow type."


-- | In LowPP/MM code, the base types for modifiable variables
--   are treated as locations, i.e., pointers.
cgIdLhs :: TVar Typ -> TVar C.Typ
cgIdLhs x = x { t_ty = Just $ cgTyp (idKind x) (getType' x) }    

           
-- | In LowPP/MM code, the base types for modifiable variables
--   need to be dereferenced before they can be used on the RHS.
cgIdRhs :: TVar Typ -> C.Exp (TVar C.Typ)
cgIdRhs x
    | isIndirect (idKind x) =
        case getType' x of
          IntTy -> C.deref (C.Var x')
          RealTy -> C.deref (C.Var x')
          _ -> C.Var x'
    | otherwise = C.Var x'
    where
      x' = x { t_ty = Just $ cgTyp (idKind x) (getType' x) }

                  
cgPrim :: Prim -> PrimMode -> [C.Exp (TVar C.Typ)] -> C.Exp (TVar C.Typ)
cgPrim prim pm es =
    case prim of
      Plus -> cgBinop C.Plus
      Minus -> cgBinop C.Minus
      Times -> cgBinop C.Times
      Div -> cgBinop C.Div
      EqEq -> cgBinop C.EqEq
      _ -> C.LibCall (getPrimLibFn pm prim) es
    where
      cgBinop bop =
          case pm of
            PM_Fn -> C.Binop (es !! 0) bop (es !! 1)
            PM_Grad _ -> C.LibCall (getPrimLibFn pm prim) es


{-| [Note]

Suppose ModData / ModParam / ModParamDup / ModAux
p_1 : Real, p_2 : Vec Real, p_3 : Mat Real

double* p_1 = v_strct.p_1;              (v_strct.p_1 : double*)
AugurVec_t* p_2 = &(v_strct.p_2.vec);   (v_strct.p_2 : AugurFlatVec_t)
AugurMat_t* p_3 = &(v_strct.p_3);       (v_strct.p_3 : AugurMat_t)

-}
projTopLvl :: Typ -> C.Exp (TVar C.Typ) -> C.Exp (TVar C.Typ)
projTopLvl ty e =
    case ty of
      VecTy _ -> C.addrOf (C.strctProj' e "vec")
      MatTy _ -> C.addrOf e
      BlkTy _ -> C.addrOf e
      _ -> e


unpackStrct :: Map.Map (TVar Typ) Int -> TVar C.Typ -> [TVar Typ] -> C.Stmt (TVar C.Typ)
unpackStrct useCtx vStrct fields =
    C.seqStmt (map f fields)
    where
      f x = let x' = cgIdLhs x
                ty = getType' x
            in
              case Map.lookup x useCtx of
                Just cnt ->
                    if cnt > 0
                    then C.assignStmt x' (projTopLvl ty (C.strctProj vStrct x'))
                    else C.mkSkip
                Nothing -> C.mkSkip
              

                 
unpackPropStrct :: TVar C.Typ -> ModParamDupCtx (TVar Typ) -> C.Stmt (TVar C.Typ)
unpackPropStrct vStrct fields =
    C.seqStmt (map f (Map.toList fields))
    where
      f (k, v) = let v' = cgIdLhs v
                     k' = cgIdLhs k
                     ty = getType' v
                 in
                   C.assignStmt v' (projTopLvl ty (C.strctProj vStrct k'))
