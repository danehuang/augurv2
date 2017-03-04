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

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts #-}

module Low.LowShpSyn where

import Data.Foldable (Foldable)
import qualified Data.Map as Map
import Data.Traversable (Traversable)
import Text.PrettyPrint
import Data.Maybe

import AstUtil.Var
import AstUtil.Pretty
import Core.CoreTySyn


----------------------------------------------------------------------
-- = LowSzSyn description
{-| [Note]

Contains shape syntax

-}  


-----------------------------------
-- == Syntax

data Shp b = Scalar                                  -- ^ "Unit"
           | SingConn (ShpExp b) (Shp b)             -- ^ Single connection 
           | MatConn (ShpExp b) (ShpExp b) (Shp b)   -- ^ Matrix connection
           | BlkOf [b]                               -- ^ Block of vars
             deriving (Show, Functor, Foldable, Traversable)
             

data ShpExp b = Cpy b                           -- ^ Copy the shape
              | Val (ConstExp b)                -- ^ Value
              | MaxDim b Int                    -- ^ Maximum dimension of i-th projection of b  
                deriving (Show, Functor, Foldable, Traversable)

data ConstExp b = Var b      -- ^ Constant variable 
                | Int Int    -- ^ Constant int
               deriving (Show, Functor, Foldable, Traversable)
                         
type ShpCtx b = Map.Map b (Shp b)

    
-----------------------------------
-- == Instances

instance (Pretty b) => Pretty (Shp b) where
    ppr Scalar = text "Scalar"
    ppr (SingConn e shp) =
        case shp of
          Scalar -> ppr e
          _ -> ppr e <+> text "x" <+> ppr shp
    ppr (MatConn row col shp) =
        case shp of
          Scalar -> brackets (ppr row <+> text "x" <+> ppr col)
          _ -> brackets (ppr row <+> text "x" <+> ppr col) <+> text "x" <+> ppr shp
    ppr (BlkOf vs) = text "Blk" <> brackets (sepBy commasp vs)

instance (Pretty b) => Pretty (ShpExp b) where
    ppr (Cpy x) = text "Cpy" <> parens (ppr x)
    ppr (Val v) = text "Val" <> parens (ppr v)
    ppr (MaxDim x i) = text "Max" <> parens (ppr x <> commasp <> int i)

instance (Pretty b) => Pretty (ConstExp b) where
    ppr (Var x) = ppr x
    ppr (Int i) = int i

             
-----------------------------------
-- == Operations on syntax
          
mkScalar :: Shp b
mkScalar = Scalar

mkCpy :: b -> Shp b
mkCpy x = SingConn (Cpy x) Scalar
           
isScalar :: Shp b -> Bool
isScalar Scalar = True
isScalar _ = False
              
isMulti :: Shp b -> Bool
isMulti = not . isScalar

tyToDims :: (TypedVar b Typ) => b -> [ShpExp b]
tyToDims x = tyToDims' x (fromJust (getType x)) 0
                    
tyToDims' :: (TypedVar b Typ) => b -> Typ -> Int -> [ShpExp b]
tyToDims' x ty idx =
    case ty of
      UnitTy -> []
      IntTy -> []
      RealTy -> []
      (VecTy ty') -> MaxDim x idx : tyToDims' x ty' (idx + 1)
      (MatTy _) ->
          -- TODO: HMMM...
          [ MaxDim x idx, MaxDim x (idx + 1) ]
      t -> error $ "@tyToDims' | Cannot infer size of type " ++ pprShow t

cpyToShp :: (TypedVar b Typ) => b -> Shp b
cpyToShp x = go (getType' x) 0
    where
      go ty idx =
          case ty of
            UnitTy -> Scalar
            IntTy -> Scalar
            RealTy -> Scalar
            VecTy ty' -> SingConn (MaxDim x idx) (go ty' (idx + 1))
            MatTy _ -> MatConn (MaxDim x idx) (MaxDim x (idx + 1)) Scalar
            t -> error $ "[LowShpSyn] @cpyToShp | Cannot infer size of type " ++ pprShow t
           
repShpFromSkel :: b -> Int -> ShpExp b -> [ShpExp b]
repShpFromSkel v len baseShp = go 0
    where
      go idx
          | idx < len = MaxDim v idx : go (idx + 1)
          | otherwise = [ baseShp ]
           
splatShpExp :: (TypedVar b Typ) => ShpExp b -> [ShpExp b]
splatShpExp (Cpy x) = tyToDims x
splatShpExp (Val e) = [ Val e ]
splatShpExp (MaxDim x axis) = [ MaxDim x axis ]
                              
shpExpsToShp :: (TypedVar b Typ) => [ShpExp b] -> Shp b
shpExpsToShp =
    foldr (\e acc -> SingConn e acc) Scalar 
                              
splatShp' :: (TypedVar b Typ) => Shp b -> [ShpExp b]
splatShp' Scalar = []
splatShp' (SingConn e shp) = splatShpExp e ++ splatShp' shp
splatShp' (MatConn row col shp) =
    -- DblConn is a matrix
    [ row, col ] ++ splatShp' shp
splatShp' (BlkOf vs) = map Cpy vs

splatShp :: (TypedVar b Typ) => Shp b -> Shp b
splatShp = shpExpsToShp . splatShp'

{-           
projShp :: (TypedVar b Typ) => Shp b -> Int -> Shp b
projShp shp proj
    | proj > 0 =
        case shp of
          Scalar -> error $ "[LowSzSyn] | Cannot project Scalar"
          SingConn _ shp' -> projShp shp' (proj - 1)
          MatConn _ col shp' -> projShp (SingConn col shp') (proj - 1)
          BlkOf _ -> error $ "[LowSzSyn] | Cannot project " ++ pprShow shp
    | otherwise = shp
-}            
projShp :: (TypedVar b Typ) => Shp b -> Int -> Shp b
projShp shp proj
    | proj > 0 =
        case shp of
          Scalar -> error $ "[LowSzSyn] | Cannot project Scalar"
          SingConn se shp' ->
              case se of
                Cpy x ->
                    case shp' of
                      Scalar -> projShp (cpyToShp x) proj
                      _ -> error $ "[LowSzSyn] | Cannot project " ++ pprShow shp
                    {-
                    case getType' x of
                      VecTy IntTy -> projShp shp' (proj - 1)
                      VecTy RealTy -> projShp shp' (proj - 1)
                      VecTy (VecTy _) -> error ""
                      VecTy (MatTy _) -> error ""
                      MatTy _ -> projShp (SingConn (MaxDim x 1) shp') (proj -1)
                      ty -> error $ "[LowSzSyn] | Cannot project type: " ++ pprShow ty
                     -}
                Val _ -> projShp shp' (proj - 1)
                MaxDim _ _ -> projShp shp' (proj - 1)
          MatConn _ col shp' -> projShp (SingConn col shp') (proj - 1)
          BlkOf _ -> error $ "[LowSzSyn] | Cannot project " ++ pprShow shp
    | otherwise = shp


lenShp :: (TypedVar b Typ) => Shp b -> Int
lenShp Scalar = 0
lenShp (SingConn _ shp) = 1 + lenShp shp
lenShp (MatConn _ _ shp) = 1 + lenShp shp
lenShp (BlkOf vs) = error $ "[LowSzSyn] @lenShp | Should not call with " ++ pprShow (BlkOf vs)
                    
isBlk :: Shp b -> Bool
isBlk (BlkOf _) = True
isBlk _ = False
                    
