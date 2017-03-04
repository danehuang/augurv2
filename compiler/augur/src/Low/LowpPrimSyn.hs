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

{-# LANGUAGE MultiParamTypeClasses #-}

module Low.LowpPrimSyn
    ( Prim(..)
    , getPrimRetTys
    , mkOverloadTys ) where

import Text.PrettyPrint

import AstUtil.Pretty
import Comm.Prim
import Compile.CompData
import Core.CoreTySyn
import AstUtil.Var


----------------------------------------------------------------------
-- = LowpPrimSyn Description
{-| [Note]

-}

compErr :: String -> a
compErr msg = compErrMod "LowpPrimSyn" msg

              
-----------------------------------
-- == Syntax

data Prim = Neg
          | Expon
          | Log
          | Expit
          | Logit
          | Plus
          | Minus
          | Times
          | Div
          | DotProd
          | EqEq
          | LAnd
          | MinusVec             -- ^ Subtract vectors
            
          -- Internal
          | SizeVec              -- ^ size of vector
          | AllocVecFromShape    -- ^ allocate vector 
          | ReadVecFromShape     -- ^ copy vector's shape
          | AllocMatFromShape    -- ^ allocate matrix 
          | ReadMatFromShape     -- ^ copy matrix's shape
          | NormAndSamp          -- ^ for discrete sampling
          | AtmIncBase           -- ^ x += 
          | AtmIncVec            -- ^ MvNormal-MvNormal conjugacy
          | AtmIncMatVTMT        -- ^ InvWishart-MvNormal conjugacy
            
          -- Special
          | MWG Name Name Name   -- ^ For MWG: proposal, swap, likelihood
          | EllipSlice Name      -- ^ For elliptical slice: likelihood
          | LeapFrog Name Name   -- ^ For HMC: gradient, likelihood
            deriving (Eq, Show)


-----------------------------------
-- == Instances
                     
instance Pretty Prim where
    ppr Neg = text "-"
    ppr Expon = text "exp"
    ppr Log = text "log"
    ppr Expit = text "expit"
    ppr Logit = text "logit"
    ppr Plus = text "+"
    ppr Minus = text "-"
    ppr Times = text "*"
    ppr Div = text "/"
    ppr DotProd = text "dotprod"
    ppr EqEq = text "=="
    ppr LAnd = text "&&"
    ppr MinusVec = text "minusVec"
    ppr SizeVec = text "sizeVec"
    ppr AllocVecFromShape = text "allocVec"
    ppr ReadVecFromShape = text "cpyVecShp"
    ppr AllocMatFromShape = text "allocMat"
    ppr ReadMatFromShape = text "cpyMatShp"
    ppr NormAndSamp = text "normAndSamp"
    ppr AtmIncBase = error "huh!??"
    ppr AtmIncVec = text "atmIncVec"
    ppr AtmIncMatVTMT = text "atmIncMatVTMT"
    ppr (MWG _ _ _) = text "mwg"
    ppr (EllipSlice _) = text "ellipSlice"
    ppr (LeapFrog _ _) = text "leapFrog"

instance Primitive Prim Typ where
    getPrimTy = getPrimTy'
    isInfix = primInfix


-----------------------------------
-- == Operations on syntax

numOverload1 :: [Typ]
numOverload1 =
    [ ArrTy [IntTy] IntTy
    , ArrTy [RealTy] RealTy ]


numOverload2 :: [Typ]
numOverload2 =
    [ ArrTy [IntTy, IntTy] IntTy
    , ArrTy [IntTy, RealTy] RealTy
    , ArrTy [RealTy, RealTy] RealTy
    , ArrTy [RealTy, RealTy] RealTy ]


vecOverload2 :: [Typ]
vecOverload2 =
    [ ArrTy [VecTy IntTy, VecTy IntTy] (VecTy IntTy)
    , ArrTy [VecTy IntTy, VecTy RealTy] (VecTy RealTy)
    , ArrTy [VecTy RealTy, VecTy RealTy] (VecTy RealTy)
    , ArrTy [VecTy RealTy, VecTy RealTy] (VecTy RealTy) ]

vecOverload2Mem :: [Typ]
vecOverload2Mem =
    [ ArrTy [VecTy IntTy, VecTy IntTy, VecTy IntTy] UnitTy
    , ArrTy [VecTy RealTy, VecTy IntTy, VecTy RealTy] UnitTy
    , ArrTy [VecTy RealTy, VecTy RealTy, VecTy RealTy] UnitTy
    , ArrTy [VecTy RealTy, VecTy RealTy, VecTy RealTy] UnitTy ]
    


getDotProdTyFn :: DopMode -> PrimMode -> [Typ]
getDotProdTyFn dm pm =
    case pm of
      PM_Fn -> 
          [ ArrTy [VecTy IntTy, VecTy IntTy] IntTy
          , ArrTy [VecTy IntTy, VecTy RealTy] RealTy
          , ArrTy [VecTy RealTy, VecTy IntTy] RealTy
          , ArrTy [VecTy RealTy, VecTy RealTy] RealTy ]
      PM_Grad _ ->
          case dm of
            DM_Fn -> [ ArrTy [RealTy, VecTy RealTy, VecTy RealTy] (VecTy RealTy) ]
            DM_Mem -> [ ArrTy [VecTy RealTy, RealTy, VecTy RealTy, VecTy RealTy] (VecTy RealTy) ]
                      

getPrimTyFn :: PrimMode -> Prim -> [Typ]
getPrimTyFn pm prim =
    case prim of
      Neg -> numOverload1
      Expon -> numOverload1
      Log -> numOverload1
      Expit -> numOverload1
      Logit -> numOverload1
      Plus -> numOverload2
      Minus -> numOverload2
      Times -> numOverload2
      Div -> numOverload2
      EqEq -> [ ArrTy [IntTy, IntTy] IntTy ]
      LAnd -> [ ArrTy [IntTy, IntTy] IntTy ]
      MinusVec -> vecOverload2
      SizeVec -> [ ArrTy [VecTy IntTy] IntTy
                 , ArrTy [VecTy RealTy] IntTy ]
      DotProd -> getDotProdTyFn DM_Fn pm
      AllocVecFromShape ->
          [ ArrTy [VecTy IntTy] (VecTy IntTy)
          , ArrTy [VecTy RealTy] (VecTy RealTy) ]
      ReadVecFromShape ->
          [ ArrTy [VecTy IntTy] (VecTy IntTy)
          , ArrTy [VecTy RealTy] (VecTy RealTy) ]
      AllocMatFromShape ->
          [ ArrTy [IntTy, IntTy] (MatTy IntTy)
          , ArrTy [IntTy, IntTy] (MatTy RealTy) ]
      ReadMatFromShape ->
          [ ArrTy [MatTy IntTy] (MatTy IntTy)
          , ArrTy [MatTy RealTy] (MatTy RealTy) ]
      NormAndSamp -> [ ArrTy [VecTy RealTy] IntTy ]
      AtmIncVec -> [ ArrTy [VecTy RealTy, VecTy RealTy] UnitTy ]
      AtmIncMatVTMT -> [ ArrTy [MatTy RealTy, VecTy RealTy] UnitTy ]
      AtmIncBase -> compErr $ "TODO type for " ++ pprShow AtmIncBase
      (MWG _ _ _) -> [ ArrTy [] UnitTy ] -- TODO: HACK
      (EllipSlice _) -> [ ArrTy [VecTy RealTy] UnitTy ] -- TODO: HACK
      (LeapFrog _ _) -> [ ArrTy [] UnitTy ] -- TODO: HACK

                        
getPrimTy' :: DopMode -> PrimMode -> Prim -> [Typ]
getPrimTy' dm pm prim =
    case dm of
      DM_Fn -> getPrimTyFn pm prim
      DM_Mem ->
          case prim of
            MinusVec -> vecOverload2Mem
            DotProd -> getDotProdTyFn dm pm
            AllocVecFromShape ->
                [ ArrTy [VecTy IntTy, VecTy IntTy] (VecTy IntTy)
                , ArrTy [VecTy RealTy, VecTy RealTy] (VecTy RealTy) ]
            ReadVecFromShape ->
                [ ArrTy [VecTy IntTy] (VecTy IntTy)
                , ArrTy [VecTy RealTy] (VecTy RealTy) ]
            AllocMatFromShape ->
                [ ArrTy [IntTy, IntTy] (MatTy IntTy)
                , ArrTy [IntTy, IntTy] (MatTy RealTy) ]
            ReadMatFromShape ->
                [ ArrTy [MatTy IntTy] (MatTy IntTy)
                , ArrTy [MatTy RealTy] (MatTy RealTy) ]
            _ -> getPrimTyFn pm prim


getPrimRetTys :: DopMode -> PrimMode -> Prim -> [Typ]
getPrimRetTys dm pm prim =
    map arrRetTy (getPrimTy' dm pm prim)


mkOverloadTys :: [Typ] -> [Typ] -> [Typ]
mkOverloadTys argTys retTys =
    map (\retTy -> ArrTy argTys retTy) retTys
        
            
primInfix :: Prim -> Bool
primInfix Plus = True
primInfix Minus = True
primInfix Times = True
primInfix Div = True
primInfix EqEq = True
primInfix _ = False

         
