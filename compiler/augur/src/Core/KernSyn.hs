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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts #-}

module Core.KernSyn where

import Text.PrettyPrint
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
    
import AstUtil.Pretty
import Core.DensSyn


----------------------------------------------------------------------
-- = KernSyn Description
{-| [Note]

Syntax for MCMC kernel.

-}


-----------------------------------
-- == Syntax
                    
newtype KernU' b = KernU' { unKern :: KernU b }

type KernU b = Kern (UserCode b) b

data UserCode b = Empty
                | Proposal (Exp b) [(b, Gen b)]

instance (Pretty b) => Pretty (UserCode b) where
    ppr Empty = text "Empty"
    ppr (Proposal e grids) =
        text "Proposal" <> braces (ppr e <+> pprGrids)
        where
          pprGrids =
              case grids of
                [] -> empty
                _ -> text "for" <+> sepBy commasp grids
                

mkUserMWG :: b -> Exp b -> [(b, Gen b)] -> KernU b
mkUserMWG v_mod e grids =
    Base (UserProp (MWG Empty Empty Empty Empty)) (Single v_mod) (dirac v_mod []) [] [] (Proposal e grids)

                
mkUserHMC :: [b] -> Maybe (Double, Double) -> KernU b
mkUserHMC v_mods opt =
    Base (GradProp (HMC Empty Empty simLen stepSize)) (Block v_mods) (prodFn (map (\v -> dirac v []) v_mods)) [] [] Empty
    where
      simLen =
          case opt of
            Just (simLen', _) -> simLen'
            Nothing -> 1.0

      stepSize =
          case opt of
            Just (_, stepSize') -> stepSize'
            Nothing -> 0.05


mkUserDiscGibbs :: b -> KernU b
mkUserDiscGibbs v_mod =
    Base (Gibbs (Disc Empty)) (Single v_mod) (dirac v_mod []) [] [] Empty


mkUserConjGibbs :: b -> KernU b
mkUserConjGibbs v_mod =
    Base (Gibbs (Conj Empty Empty)) (Single v_mod) (dirac v_mod []) [] [] Empty


mkUserESlice :: b -> KernU b
mkUserESlice v_mod =
    Base (Slice (Ellip Empty Empty)) (Single v_mod) (dirac v_mod []) [] [] Empty
         
                
data Kern code b = Base (KernKind code) (KernUnit b) (Fn b) [(b, AllocKind)] [b] code
                 -- ^ kernel kind, kernel unit, unnormalized full-cond, kernel allocs, kernel parameters, full-cond likelihood code
              
                 | Tensor (Kern code b) (Kern code b)
                   deriving (Show, Functor, Foldable, Traversable)

                
data KernUnit b = Single b
                | Block [b]
                  deriving (Show, Functor, Foldable, Traversable)

                           
data KernKind code = UserProp (PropKind code)
                   | GradProp (GradKind code)
                   | Gibbs (GibbsKind code)
                   | Slice (SliceKind code)
                     deriving (Show)

data AllocKind = Reset
               | Work
                 deriving (Show)
                              
data PropKind code = Joint code           -- ^ proposal
                   | MWG code code code code  -- ^ proposal, swap, likelihood, top-level
                     deriving (Show)

                         
data GradKind code = HMC code code Double Double  -- ^ gradient, proposal
                   | Reflect code         -- ^ gradient
                deriving (Show)

data GibbsKind code = Disc code           -- ^ sample
                    | Conj code code      -- ^ statistic, sample
                      deriving (Show)
                         
                       
data SliceKind code = Ellip code code     -- ^ individual likelihood, proposal
               deriving (Show)


-----------------------------------
-- == Instances


-- === Pretty

instance (Pretty b) => Pretty (KernU' b) where
    ppr (KernU' k) = f k
        where
          f (Base kind ku fcFn _ _ code) =
              vcat [ text "Kernel: " <+> ppr kind <> parens (ppr ku)
                   , text "Full-cond: " <+> ppr fcFn
                   , text "Code: " <+> ppr code ]
          f (Tensor k1 k2) =
              vcat [ f k1 , text "(*)", f k2 ]


instance (Pretty b, Pretty code) => Pretty (Kern b code) where
    ppr (Base kind ku fcFn allocs kernParams code) =
        vcat [ ppr kind <> parens (ppr ku), ppr fcFn, sepBy commasp allocs, ppr code ]
    ppr (Tensor k1 k2) =
        vcat [ ppr k1 , text "(*)", ppr k2 ]
             
instance (Pretty b) => Pretty (KernUnit b) where
    ppr (Single x) = ppr x
    ppr (Block xs) = sepBy commasp xs

instance Pretty AllocKind where
    ppr Reset = text "reset"
    ppr Work = text "work"
                     
instance Pretty (KernKind code) where
    ppr (UserProp pk) = ppr pk <> text "-MH"
    ppr (GradProp gk) = ppr gk <> text "-MH"
    ppr (Gibbs gk) = ppr gk <> text "-Gibbs"
    ppr (Slice sk) = ppr sk <> text "-Slice"

instance Pretty (PropKind code) where
    ppr (Joint _) = text "Joint"
    ppr (MWG _ _ _ _) = text "MWG"
                     
instance Pretty (GradKind code) where
    ppr (HMC _ _ _ _) = text "HMC"
    ppr (Reflect _) = text "Reflect"

instance Pretty (GibbsKind code) where
    ppr (Disc _) = text "Disc"
    ppr (Conj _ _) = text "Conj"
              
instance Pretty (SliceKind code) where
    ppr (Ellip _ _) = text "Ellip"

                
-----------------------------------
-- == Operations on Syntax

kuVars :: KernUnit b -> [b]
kuVars (Single x) = [x]
kuVars (Block xs) = xs


mapCode :: (c1 -> c2) -> Kern c1 b -> Kern c2 b
mapCode f (Base kind ku fc allocs kernParams like) =
    case kind of
      UserProp pk ->
          case pk of
            Joint prop -> Base (UserProp (Joint (f prop))) ku fc allocs kernParams (f like)
            MWG prop swap like top -> Base (UserProp (MWG (f prop) (f swap) (f like) (f top))) ku fc allocs kernParams (f like)
      GradProp gk ->
          case gk of
            HMC grad prop simLen stepSize -> Base (GradProp (HMC (f grad) (f prop) simLen stepSize)) ku fc allocs kernParams (f like)
            Reflect grad -> Base (GradProp (Reflect (f grad))) ku fc allocs kernParams (f like)
      Gibbs gk ->
          case gk of
            Disc samp -> Base (Gibbs (Disc (f samp))) ku fc allocs kernParams (f like)
            Conj stat samp -> Base (Gibbs (Conj (f stat) (f samp))) ku fc allocs kernParams (f like)
      Slice sk ->
          case sk of
            Ellip like' prop -> Base (Slice (Ellip (f like') (f prop))) ku fc allocs kernParams (f like)
mapCode f (Tensor k1 k2) = Tensor (mapCode f k1) (mapCode f k2)

                           
gatherCode :: Kern c b -> [c]
gatherCode (Base kind _ _ _ _ like) =
    case kind of
      UserProp pk ->
          case pk of
            Joint prop -> [ like, prop ]
            MWG prop swap like' top -> [ prop, swap, like', top ]
      GradProp gk ->
          case gk of
            HMC grad prop _ _ -> [ like, grad, prop ]
            Reflect grad -> [ like, grad ]
      Gibbs gk ->
          case gk of
            Disc samp -> [ samp ]
            Conj stat samp -> [ stat, samp ]
      Slice sk ->
          case sk of
            Ellip like' prop -> [ like', prop ]
gatherCode (Tensor k1 k2) =
    gatherCode k1 ++ gatherCode k2


gatherKernParams :: Kern c b -> [b]
gatherKernParams (Base _ _ _ _ kernParams _) =
    kernParams
gatherKernParams (Tensor k1 k2) =
    gatherKernParams k1 ++ gatherKernParams k2


needProp :: Kern b c -> Bool
needProp (Base kind _ _ _ _ _) =
    case kind of
      UserProp _ -> True
      GradProp _ -> True
      Gibbs _ -> False
      Slice _ -> True
needProp (Tensor k1 k2) = needProp k1 || needProp k2
