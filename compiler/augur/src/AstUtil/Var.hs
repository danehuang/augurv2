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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module AstUtil.Var where

import Data.IORef
import Data.Maybe
import Data.Traversable(Traversable, mapAccumL)
import Text.PrettyPrint
import qualified Data.Map as Map
    
import AstUtil.Fresh
import AstUtil.Pretty

    
----------------------------------------------------------------------
-- = Var Description
{-| [Note]

This module contains representations of variables. 
1) Var: base variable that is named
2) MVar: mutable variable that is used to give unique ids
3) TVar: typed variable that has a unique id

-}
    

-----------------------------------
-- == Syntax

data Var =
    Id { v_varName :: !String            -- ^ variable name
       , v_idKind :: IdKind              -- ^ kind of identifier
       }

data MVar =
    MId { m_uid :: !Int                  -- ^ unique id
        , m_uidRef :: IORef (Maybe Int)  -- ^ for unification purposes
        , m_varName :: !Name             -- ^ variable name
        , m_idKind :: IdKind             -- ^ kind of identifier
        }

data TVar t =
    TId { t_uid :: !Int                  -- ^ unique id
        , t_varName :: !Name             -- ^ variable name
        , t_idKind :: IdKind             -- ^ kind of identifier
        , t_packed :: Maybe String       -- ^ its memory region, TODO (poorly named)
        , t_ty :: Maybe t                -- ^ its type
        }
    
data Name = Anon                         -- ^ anonymous
          | Name !String                 -- ^ name
          | CompName !String !Name       -- ^ compound name
    
data IdKind = ModParam ParamKind    -- ^ model parameter (what we are trying to fit)
            | ModParamDup  -- ^ model parameter duplicate (for proposals)
            | ModData      -- ^ model data (i.e., input)
            | ModHyper     -- ^ model hyper-parameters
            | ModAux       -- ^ model auxilliary variables (e.g., compiler
                           -- ^ generated auxilliary inference variables)
            | Local        -- ^ local variable
            | Param        -- ^ function parameter
              deriving (Show, Eq)

data ParamKind = PK_Det
               | PK_Prob
                 deriving (Show, Eq)
                       

-----------------------------------
-- == Classes
                          
class Sanitizable a where
    sanitize :: a -> a
    unsanitize :: a -> a

class (Ord b, Pretty b, Sanitizable b) => BasicVar b where
    idKind :: b -> IdKind
    varName :: b -> Name
    mkIdIO :: GenSym -> Name -> IdKind -> IO b
    
class (BasicVar b) => TypedVar b t | b -> t where
    getUid :: b -> Int
    getType :: b -> Maybe t
    setType :: b -> t -> b
    mkTyIdIO :: GenSym -> Name -> IdKind -> t -> IO b

    getType' :: b -> t
    getType' = fromJust . getType
               
{-              
class (Ord a, Pretty a, Sanitizable a) => AugurVar a where
    idKind :: a -> IdKind
    varName :: a -> Name
    mkIdIO :: GenSym -> Name -> IdKind -> IO a
              
    mkIdIOAnon :: GenSym -> IdKind -> IO a
    mkIdIOAnon genSym ik = mkIdIO genSym Anon ik
-}


-----------------------------------
-- == Instances

instance Show Name where
    show Anon = "anon"
    show (Name str) = str
    show (CompName pre rest) = pre ++ "_" ++ show rest

instance Show Var where
    show (Id{v_varName=s, v_idKind=ik}) =
        "<" ++ show s ++ ", " ++ show ik ++ ">"
                               
instance Show MVar where
    show (MId{m_uid=uid, m_uidRef=_, m_varName=vn, m_idKind=ik}) =
        "<" ++ show uid ++ "," ++ show vn ++ "," ++ show ik ++ ">"

instance (Show t) => Show (TVar t) where
    show (TId{t_uid=uid, t_varName=vn, t_idKind=ik, t_packed=op, t_ty=ot}) =
        case ot of
          Nothing -> "<" ++ show uid ++ "," ++ show vn ++ " :: ?"
                     ++ "," ++ pre op ++ show ik ++ ">"
          Just ty -> "<" ++ show uid ++ "," ++ show vn ++ " :: " ++ show ty
                    ++ "," ++ pre op ++ show ik ++ ">"
        where
          pre Nothing = ""
          pre (Just s) = s ++ ","
                                                  

instance Eq Name where
    (==) (Name n1) (Name n2) = n1 == n2
    (==) (CompName b1 n1) (CompName b2 n2) = b1 == b2 && n1 == n2
    (==) _ _ = False

instance Eq Var where
    (==) x1 x2 = varName x1 == varName x2

instance Eq MVar where
    (==) x1 x2 = m_uid x1 == m_uid x2

instance Eq (TVar t) where
    (==) x1 x2 = t_uid x1 == t_uid x2
                

instance Ord Var where
    (<=) x1 x2 = f (varName x1) (varName x2)
        where
          f (Name s1) (Name s2) = s1 <= s2
          f n1 n2 = error $ "Comparing: " ++ show n1 ++ " with " ++ show n2
  
instance Ord MVar where
    (<=) x1 x2 = m_uid x1 <= m_uid x2
                     
instance Ord (TVar t) where
    (<=) x1 x2 = t_uid x1 <= t_uid x2

                         
instance Sanitizable String where
    sanitize str = "_" ++ str

    unsanitize (_:str) = str
    unsanitize [] = []

instance Sanitizable Name where
    sanitize Anon = Anon
    sanitize (Name str) = Name (sanitize str)
    sanitize (CompName pre n) = CompName (sanitize pre) n

    unsanitize Anon = Anon
    unsanitize (Name str) = Name (unsanitize str)
    unsanitize (CompName pre n) = CompName (unsanitize pre) n

instance Sanitizable Var where
    sanitize (Id n ik) = Id (sanitize n) ik
    unsanitize (Id n ik) = Id (unsanitize n) ik
                                  
instance Sanitizable MVar where
    sanitize (MId uid uidRef n ik) = MId uid uidRef (sanitize n) ik
    unsanitize (MId uid uidRef n ik) = MId uid uidRef (unsanitize n) ik

instance Sanitizable (TVar t) where
    sanitize (TId uid n ik packed ty) = TId uid (sanitize n) ik packed ty
    unsanitize (TId uid n ik packed ty) = TId uid (unsanitize n) ik packed ty

                                          
instance BasicVar Var where
    idKind v = v_idKind v               
    varName v = Name $ v_varName v                
    mkIdIO _ (Name s) ik = return $ Id s ik
    mkIdIO _ name _ =
        error $ "@mkIdIO | Cannot pass " ++ show name ++ " to mkIdIO for Var"
              
instance BasicVar MVar where
    idKind v = m_idKind v
    varName v = m_varName v
    mkIdIO genSym n ik =
        do sym <- freshSym genSym           
           mid <- newIORef (Just sym)
           return $ MId sym mid n ik
                  
instance BasicVar (TVar t) where
    idKind v = t_idKind v
    varName v = t_varName v    
    mkIdIO genSym n ik =
        do sym <- freshSym genSym
           return $ TId sym n ik Nothing Nothing

instance TypedVar (TVar t) t where
    getUid = t_uid
    getType = t_ty
    setType x ty = x { t_ty = Just ty }
    mkTyIdIO genSym name ik ty =
        do sym <- freshSym genSym
           return $ TId sym name ik Nothing (Just ty)
     
instance Pretty IdKind where
    ppr (ModParam pk) = text "Param" <> text "_" <> ppr pk
    ppr ModParamDup = text "ParamDup"
    ppr ModData = text "Data"
    ppr ModHyper = text "Hyper"
    ppr ModAux = text "Aux"
    ppr Local = text "Local"
    ppr Param = text "FnParam"

instance Pretty ParamKind where
    ppr PK_Det = text "Det"
    ppr PK_Prob = text "Prob"
                
instance Pretty Name where
    ppr Anon = text "anon"
    ppr (Name str) = text str
    ppr (CompName pre rest) = text pre <> text "_" <> ppr rest

instance Pretty Var where
    ppr (Id name _) = text name
                              
instance Pretty MVar where
    ppr (MId uid _ name _) =
        case name of
          Anon -> ppr name <> int uid
          Name _ -> ppr name
          CompName _ _ -> ppr name

instance Pretty (TVar t) where
    ppr (TId uid name ik packed _) =
        case ik of
          (ModParam _) -> ppr name
          ModParamDup -> text "m" <> int uid
          ModData -> ppr name
          ModHyper -> ppr name
          ModAux ->
              case name of
                Anon -> text "g" <> int uid
                _ -> ppr name
          Local -> text "t" <> int uid
          Param -> ppr name


-----------------------------------
-- == Operations on syntax
                   
-- ** Name
              
nameToStr :: Name -> String
nameToStr Anon = "@anon"
nameToStr (Name str) = str
nameToStr (CompName pre rest) = pre ++ "_" ++ nameToStr rest
                          
mkAnon :: Name
mkAnon = Anon
                                
mkName :: String -> Name
mkName x = Name x

mkCompName :: String -> Name -> Name
mkCompName pre rest = CompName pre rest


-- ** IdKind
                      
isModParam :: IdKind -> Bool
isModParam (ModParam _) = True
isModParam _ = False

isModData :: IdKind -> Bool
isModData ModData = True
isModData _ = False

isModAux :: IdKind -> Bool
isModAux ModAux = True
isModAux _ = False

isModHyper :: IdKind -> Bool
isModHyper ModHyper = True
isModHyper _ = False
             
isModLocal :: IdKind -> Bool
isModLocal Local = True
isModLocal _ = False

isModDecl :: IdKind -> Bool
isModDecl (ModParam _) = True
isModDecl ModData = True
isModDecl _ = False              
              

-- ** Vars
              
setPacked :: Maybe String -> TVar t -> TVar t
setPacked p v = v { t_packed = p }

pack :: Maybe String -> Maybe String -> TVar t -> TVar t
pack os os' v =
    case idKind v of
      (ModParam _) -> setPacked os v
      ModParamDup -> setPacked os v
      ModData -> setPacked os v
      ModHyper -> setPacked os' v
      ModAux -> setPacked os' v
      Local -> setPacked Nothing v
      Param -> setPacked Nothing v
                              
midToTid :: MVar -> TVar t
midToTid v = TId (m_uid v) (m_varName v) (m_idKind v) Nothing Nothing

tidToMid :: TVar t -> IO MVar
tidToMid v =
    do uidRef <- newIORef (Just (t_uid v))
       return $ MId (t_uid v) uidRef (t_varName v) (t_idKind v)
              
mkTId :: GenSym -> Name -> IdKind -> t -> IO (TVar t)
mkTId genSym n ik ty =
    do ctr <- readIORef genSym
       writeIORef genSym (ctr + 1)
       return $ TId ctr n ik Nothing (Just ty)

              
blkName :: (TypedVar b t) => [b] -> Name
blkName vs =
    mkCompName "blk" (mkName (concat (map (nameToStr . varName) vs)))

blkName' :: (TypedVar b t) => String -> [b] -> Name
blkName' pre vs =
    mkCompName pre (mkName (concat (map (nameToStr . varName) vs)))

               
cntVarUse :: (BasicVar b, Traversable t) => t b -> Map.Map b Int
cntVarUse syntax =
    fst $ mapAccumL (\acc v -> (extend v acc, ())) Map.empty syntax
    where
      extend v ctx =
          case Map.lookup v ctx of
            Just cnt -> Map.insert v (cnt + 1) ctx
            Nothing -> Map.insert v 1 ctx
