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

module Comm.DistSyn where
    
import Text.PrettyPrint

import AstUtil.Pretty
import Comm.TypSyn
import Comm.Prim


----------------------------------------------------------------------
-- = DistSyn Description
{-| [Note]

Contains distribution syntax.

-}    


-----------------------------------
-- == Syntax

data Dist = Dirac

          -- Discrete
          | Bernoulli
          | Categorical
          | Geometric
          | Poisson

          -- Continuous
          | Beta
          | Exponential
          | Gamma
          | InvGamma
          | Normal
          | Uniform

          -- Multi-variate
          | Dirichlet
          | MvNormal
          | InvWishart
            deriving (Eq, Show)

data Dop = LL        
         | Pdf
         | Sample
         | Conj Dist (Maybe String)  -- TODO: what!?
         | DotPt     
         | DotArg1
         | DotArg2
           deriving (Eq, Show)

                        
-----------------------------------
-- == Instances
               
instance Pretty Dist where
    ppr Dirac = text "Dirac"
    ppr Bernoulli = text "Bernoulli"
    ppr Categorical = text "Categorical"
    ppr Geometric = text "Geometric"
    ppr Poisson = text "Poisson"
    ppr Beta = text "Beta"
    ppr Exponential = text "Exponential"
    ppr Gamma = text "Gamma"
    ppr InvGamma = text "InvGamma"
    ppr Normal = text "Normal"
    ppr Uniform = text "Uniform"
    ppr Dirichlet = text "Dirichlet"
    ppr MvNormal = text "MvNormal"
    ppr InvWishart = text "InvWishart"
                     
instance Pretty Dop where
    ppr LL = text "ll"
    ppr Pdf = text "pdf"
    ppr Sample = text "sample"
    ppr (Conj _ _) = text "conj"
    ppr DotPt = text "dotpt"
    ppr DotArg1 = text "dotarg1"
    ppr DotArg2 = text "dotarg2"

                  
-----------------------------------
-- == Operations on syntax

distArgTys :: Dist -> [Typ]
distArgTys Dirac = error $ "Should not call distArgTys with Dirac"
distArgTys Bernoulli = [ RealTy ]
distArgTys Categorical = [ VecTy RealTy ]
distArgTys Geometric = [ RealTy ]
distArgTys Poisson = [ RealTy ]
distArgTys Beta = [ RealTy, RealTy ]
distArgTys Exponential = [ RealTy ]
distArgTys Gamma = [ RealTy, RealTy ]
distArgTys InvGamma = [ RealTy, RealTy ]
distArgTys Normal = [ RealTy, RealTy ]
distArgTys Uniform = [ RealTy, RealTy ]
distArgTys Dirichlet = [ VecTy RealTy ]
distArgTys MvNormal = [ VecTy RealTy, MatTy RealTy ]
distArgTys InvWishart = [ IntTy, MatTy RealTy ]
                        
distSampTy :: Dist -> Typ
distSampTy Dirac = error $ "Should not call distSampTy with Dirac"
distSampTy Bernoulli = IntTy
distSampTy Categorical = IntTy
distSampTy Geometric = IntTy
distSampTy Poisson = IntTy
distSampTy Beta = RealTy
distSampTy Exponential = RealTy
distSampTy Gamma = RealTy
distSampTy InvGamma = RealTy
distSampTy Normal = RealTy
distSampTy Uniform = RealTy
distSampTy Dirichlet = VecTy RealTy
distSampTy MvNormal = VecTy RealTy
distSampTy InvWishart = MatTy RealTy

isScl :: Dist -> Bool
isScl dist =
    case distSampTy dist of
      IntTy -> True
      RealTy -> True
      VecTy _ -> False
      MatTy _ -> False

isMv :: Dist -> Bool
isMv = not . isScl
                  
isDisc :: Dist -> Bool
isDisc dist =
    case distSampTy dist of
      IntTy -> True
      RealTy -> False
      VecTy _ -> False
      MatTy _ -> False

isCont :: Dist -> Bool
isCont = not . isDisc

         
numArgs :: Dist -> Int
numArgs = length . distArgTys

          
hasPt :: Dop -> Bool
hasPt LL = True
hasPt Pdf = True
hasPt Sample = False
hasPt (Conj _ _) = False
hasPt DotPt = True
hasPt DotArg1 = True
hasPt DotArg2 = True
                

                
-- | Expose working memory, but not destination memory
memDistArgTys :: Dist -> [Typ]
memDistArgTys Dirac = error $ "Should not call memDistArgTys with Dirac"
memDistArgTys Dirichlet = [ VecTy RealTy ]
memDistArgTys MvNormal = [ VecTy RealTy, MatTy RealTy, MatTy RealTy, VecTy RealTy ]
memDistArgTys InvWishart = [ IntTy, MatTy RealTy, MatTy RealTy, MatTy RealTy ]
memDistArgTys dist = distArgTys dist

                     
memDistSampTy :: Dist -> Typ
memDistSampTy Dirac = error $ "Should not call memDistSampTy with Dirac"
memDistSampTy Dirichlet = UnitTy 
memDistSampTy MvNormal = UnitTy
memDistSampTy InvWishart = UnitTy
memDistSampTy dist = distSampTy dist


distRetTy' :: Dop -> DopMode -> Dist -> Typ
distRetTy' dop dm dist =
    case dm of
      DM_Fn ->
          case dop of
            LL -> RealTy
            Pdf -> RealTy
            Sample -> distSampTy dist
            Conj _ _ -> distSampTy dist
            DotPt -> distSampTy dist
            DotArg1 -> distArgTys dist !! 0
            DotArg2 -> distArgTys dist !! 1
      DM_Mem ->
          case dop of
            LL -> RealTy
            Pdf -> RealTy
            Sample -> memDistSampTy dist
            Conj _ _ -> memDistSampTy dist
            DotPt -> memDistSampTy dist
            DotArg1 -> memDistArgTys dist !! 0
            DotArg2 -> memDistArgTys dist !! 1

                       
distArgTys' :: Dop -> DopMode -> Dist -> [Typ]
distArgTys' dop dm dist =
    case dm of
      DM_Fn ->
          case dop of
            LL -> distSampTy dist : distArgTys dist
            Pdf -> distSampTy dist : distArgTys dist
            Sample -> distArgTys dist
            Conj _ _ -> error $ "[DistSyn] | TODO"
            DotPt -> distSampTy dist : distArgTys dist
            DotArg1 -> distSampTy dist : distArgTys dist
            DotArg2 -> distSampTy dist : distArgTys dist
      DM_Mem -> 
          case dop of
            LL -> distSampTy dist : memDistArgTys dist
            Pdf -> distSampTy dist : memDistArgTys dist
            Sample ->
                if isScl dist
                then distArgTys dist
                else distSampTy dist : memDistArgTys dist
            Conj _ _ -> error $ "[DistSyn] | TODO"
            DotPt ->
                if isScl dist
                then distSampTy dist : memDistArgTys dist
                else distSampTy dist : distSampTy dist : memDistArgTys dist
            DotArg1 ->
                if isScl dist
                then distSampTy dist : memDistArgTys dist
                else memDistArgTys dist !! 0 : distSampTy dist : memDistArgTys dist
            DotArg2 ->
                if isScl dist
                then distSampTy dist : memDistArgTys dist
                else memDistArgTys dist !! 1 : distSampTy dist : memDistArgTys dist
