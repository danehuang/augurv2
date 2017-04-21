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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Core.CoreTySyn where

import Control.Monad.Except
import Data.Foldable (Foldable)
import qualified Data.Map as Map
import Data.Traversable (Traversable)
import Text.PrettyPrint
    
import AstUtil.Pretty
import AstUtil.VarOp
import qualified Comm.TypSyn as C


----------------------------------------------------------------------
-- = CoreTySyn Description
{-| [Note]

Core type syntax.

-}


data Typ = UnitTy
         | IntTy
         | RealTy
         | VecTy Typ
         | MatTy Typ
         | BlkTy [Typ]
         | ArrTy [Typ] Typ
           deriving (Eq, Show)

type TvId = Int                    
data Tipe tv = UnitTi
             | IntTi
             | RealTi
             | VecTi (Tipe tv)
             | MatTi (Tipe tv)
             | ArrTi [Tipe tv] (Tipe tv)
             | TiVar tv
             | MeetTi (Tipe tv) (Tipe tv)
               deriving (Show, Functor, Foldable, Traversable)

data Cstr tv = TiEq (Tipe tv) (Tipe tv)
             | SubTi (Tipe tv) (Tipe tv)
               deriving (Show, Functor, Foldable, Traversable)

newtype TyCtx b tv = TyCtx { unTyCtx :: (Map.Map b (Tipe tv)) }

{-    
class Tipeable a where
    subst :: TyCtx a -> 
-}



----------------------------------------------------------------------
-- * Instances
----------------------------------------------------------------------
                    
instance Pretty Typ where
    ppr UnitTy = text "Unit"
    ppr IntTy = text "Int"
    ppr RealTy = text "Real"
    ppr (VecTy t) = parens (text "Vec" <+> ppr t)
    ppr (MatTy t) = parens (text "Mat" <+> ppr t)
    ppr (BlkTy ts) = text "Blk" <> parens (sepBy commasp ts)
    ppr (ArrTy ts t) = parens (sepBy (text "*") ts <+> text "->" <+> ppr t)

         
instance (Pretty b) => Pretty (Tipe b) where
    ppr UnitTi = text "unit"
    ppr IntTi = text "int"
    ppr RealTi = text "real"
    ppr (VecTi t) = text "vec<" <> ppr t <> text ">"
    ppr (MatTi t) = text "mat<" <> ppr t <> text ">"
    ppr (ArrTi ts t) = parens (sepBy (text "*") ts <+> text "->" <+> ppr t)
    ppr (TiVar tv) = ppr tv
    ppr (MeetTi t1 t2) = parens (ppr t1 <+> text "/\\" <+> ppr t2)
                    
instance (Pretty b) => Pretty (Cstr b) where
    ppr (TiEq t1 t2) = ppr t1 <+> text "=" <+> ppr t2
    ppr (SubTi t1 t2) = ppr t1 <+> text "<:" <+> ppr t2

                        
instance (Ord tv) => Substitutable tv (Tipe tv) (Tipe tv) where
    substP p x term = sub
        where
          sub UnitTi = UnitTi
          sub IntTi = IntTi
          sub RealTi = RealTi
          sub (VecTi t) = VecTi (sub t)
          sub (MatTi t) = MatTi (sub t)
          sub (ArrTi ts t) = ArrTi (map sub ts) (sub t)
          sub (TiVar y) | x `p` y = term
                        | otherwise = TiVar y
          sub (MeetTi t1 t2) = MeetTi (sub t1) (sub t2)

instance (Ord tv) => Substitutable tv (Cstr tv) (Tipe tv) where
    substP p x term = sub
        where
          sub (TiEq t1 t2) = TiEq (substP p x term t1) (substP p x term t2)
          sub (SubTi t1 t2) = SubTi (substP p x term t1) (substP p x term t2)

{-                              
instance (Ord b, Ord tv) => Substitutable tv (TyCtx b tv) (Tipe tv) where
    substP p x term tyCtx =
        TyCtx (Map.map (substP p x term) (unTyCtx tyCtx))
-}
              
type Tipe' = Tipe TvId
type Cstr' = Cstr TvId
              
instance (Ord b) => Substitutable TvId (TyCtx b TvId) (Tipe TvId) where
    substP p x term tyCtx =
        TyCtx (Map.map (substP p x term) (unTyCtx tyCtx))


              
----------------------------------------------------------------------
-- * Operations
----------------------------------------------------------------------

joinNumTy :: Typ -> Typ -> Either String Typ
joinNumTy IntTy IntTy = return IntTy
joinNumTy IntTy RealTy = return RealTy
joinNumTy RealTy IntTy = return RealTy
joinNumTy RealTy RealTy = return RealTy
joinNumTy (VecTy t1) (VecTy t2) =
    do t <- joinNumTy t1 t2
       return $ VecTy t
joinNumTy (MatTy t1) (MatTy t2) =
    do t <- joinNumTy t1 t2
       return $ MatTy t
joinNumTy t1 t2 =
    throwError $ "Cannot take join of " ++ pprShow t1 ++ " with " ++ pprShow t2

meetNumTy :: Typ -> Typ -> Either String Typ
meetNumTy IntTy IntTy = return IntTy
meetNumTy IntTy RealTy = return IntTy
meetNumTy RealTy IntTy = return IntTy
meetNumTy RealTy RealTy = return RealTy
meetNumTy (VecTy t1) (VecTy t2) =
    do t <- meetNumTy t1 t2
       return $ VecTy t
meetNumTy (MatTy t1) (MatTy t2) =
    do t <- meetNumTy t1 t2
       return $ MatTy t
meetNumTy t1 t2 =
    throwError $ "Cannot take meet of " ++ pprShow t1 ++ " with " ++ pprShow t2

joinNumTys :: [Typ] -> Either String Typ
joinNumTys [] = throwError $ "Taking join of empty list"
joinNumTys (t:[]) = return t
joinNumTys ts = foldM (\acc t -> joinNumTy acc t) (head ts) (tail ts)

joinNumTys' :: [Typ] -> Typ
joinNumTys' ts =
    case joinNumTys ts of
      Left errMsg -> error errMsg
      Right t -> t
                
meetNumTys :: [Typ] -> Either String Typ
meetNumTys [] = throwError $ "Taking meet of empty list"
meetNumTys (t:[]) = return t
meetNumTys ts = foldM (\acc t -> meetNumTy acc t) (head ts) (tail ts)

meetNumTys' :: [Typ] -> Typ
meetNumTys' ts =
    case meetNumTys ts of
      Left errMsg -> error errMsg
      Right t -> t
                
injTy :: Typ -> Tipe b
injTy UnitTy = UnitTi
injTy IntTy = IntTi
injTy RealTy = RealTi
injTy (VecTy t) = VecTi (injTy t)
injTy (MatTy t) = MatTi (injTy t)
injTy (ArrTy ts t) = ArrTi (map injTy ts) (injTy t)

projTy :: (Pretty b) => Tipe b -> Either String Typ
projTy UnitTi = return UnitTy
projTy IntTi = return IntTy
projTy RealTi = return RealTy
projTy (VecTi t) = projTy t >>= \t' -> return (VecTy t')
projTy (MatTi t) = projTy t >>= \t' -> return (MatTy t')
projTy (ArrTi ts t) =
    do ts' <- mapM projTy ts
       t' <- projTy t
       return $ ArrTy ts' t'
projTy ty@(TiVar _) =
    throwError $ "@projTy | cannot project: " ++ pprShow ty
projTy ty@(MeetTi _ _) =
    throwError $ "@projTy | cannot project: " ++ pprShow ty
               

injCommTy :: C.Typ -> Typ
injCommTy C.UnitTy = UnitTy
injCommTy C.IntTy = IntTy
injCommTy C.RealTy = RealTy
injCommTy (C.VecTy t) = VecTy (injCommTy t)
injCommTy (C.MatTy t) = MatTy (injCommTy t)
injCommTy (C.ArrTy ts t) = ArrTy (map injCommTy ts) (injCommTy t)

mkMeetTi :: [Tipe tv] -> Tipe tv
mkMeetTi (hd:[]) = hd
mkMeetTi (hd:tl) = MeetTi hd (mkMeetTi tl)
mkMeetTi [] = error $ "Should not call mkMeet with 0-length list"


numTy :: [Typ]
numTy = [ IntTy, RealTy ]

numTy1 :: [[Typ]]
numTy1 =
    do t <- numTy
       return [ t ]

numTy2 :: [[Typ]]
numTy2 =
    do t1 <- numTy
       t2 <- numTy
       return [ t1, t2 ]

overload1 :: [Typ]
overload1 = map (\ts -> ArrTy ts RealTy) numTy1

overload2 :: [Typ]
overload2 = map (\ts -> ArrTy ts (joinNumTys' ts)) numTy2

arrRetTy :: Typ -> Typ
arrRetTy (ArrTy _ t) = t
arrRetTy t = error $ "Should not call arrRetTy with " ++ pprShow t


projBaseTy :: Typ -> [a] -> Typ
projBaseTy t ls
    | length ls > 0 =
        case t of
          VecTy t' -> projBaseTy t' (tail ls)
          MatTy t' -> projBaseTy (VecTy t') (tail ls)
          _ -> error $ "Shouldn't happen"
    | otherwise = t


isBlkTy :: Typ -> Bool
isBlkTy (BlkTy _) = True
isBlkTy _ = False
