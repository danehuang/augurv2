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

module Rv.LowerRv
    ( tyGrid, piGrid
    , lowerProg
    , runLowerProg
    , runLowerUserCode ) where
    
import Control.Monad.Except

import AstUtil.Var
import AstUtil.Pretty
import Rv.RvSyn
import Core.DensSyn
import Core.CoreSyn
import Core.KernSyn
import Core.CoreTySyn
import Comm.DistSyn
import Compile.CompData

   
----------------------------------------------------------------------
-- = LowerRv Description
{-| [Note]

Lower to Core IL.

-}

    
-----------------------------------
-- == Types and operations

type LowM = CompM



tyGrid :: Typ -> [(String, Gen String)] -> Typ
tyGrid ty = go
    where
      go (_ : tl) = VecTy (go tl)
      go [] = ty

piGrid :: Fn b -> [(b, Gen b)] -> Fn b
piGrid fn = go
    where
      go ((idx, gen) : tl) = Pi idx gen (go tl)
      go [] = fn


              
-----------------------------------
-- == Transformation

lowerModBody :: IdKind -> Typ -> String -> [(String, Gen String)] -> Exp String -> LowM (ModDecl String, Fn String)
lowerModBody ik ty name grid body =
    case body of
      DistOp _ Dirac es ->
          do let modDecl = (ik, name, tyGrid ty grid)
             return (modDecl, piGrid (Dens Dirac (mkPt (map fst grid)) es) grid)
      DistOp _ dist es ->
          do let modDecl = (ik, name, tyGrid (injCommTy (distSampTy dist)) grid)
             return (modDecl, piGrid (Dens dist (mkPt (map fst grid)) es) grid)
      _ -> throwError $ "[LowerRv] @lowerModBody | Expected distribution but found " ++ show body
    where
      mkPt [] = Var name
      mkPt idxs = Proj (Var name) (map Var idxs)
              
lowerDecl :: Decl -> LowM (ModDecl String, Fn String)
lowerDecl (Decl name ty ik grid body) =
    case ik of
      ModParam _ -> lowerModBody ik ty name grid body
      ModData -> lowerModBody ik ty name grid body
      _ -> error $ "[LowerRv] @lowerModBody | Shouldn't find " ++ pprShow ik ++ " when translating model parameter or data"


-----------------------------------
-- == Top-level
           
lowerProg :: Model -> LowM (ModDecls String, Fn String)
lowerProg model =
    do (decls, fns) <- mapM lowerDecl (model_decls model) >>= return . unzip
       let fn = prodFn fns
       debugM "[Rv.LowerRv]" $ "Output (Density): " ++ pprShow fn
       return (decls, fn)

              
runLowerProg :: Model -> IO (Either String (ModDecls String, Fn String))
runLowerProg = runComp . lowerProg
              

lowerUserCode :: (BasicVar b) => Fn b -> UserCode b -> LowM (Fn b)
lowerUserCode _ Empty = throwError $ "Cannot generate proposal with empty code"
lowerUserCode fnPt (Proposal e grids) =
    case e of
      DistOp _ dist es ->
          do let pt = gatherDensPt fnPt              
                 pt' = mkDensPt (densPtVar pt) (map (Var . fst) grids)
             return $ piGrid (Dens dist pt' es) grids
      _ -> throwError $ "Ill-formed proposal code, expected distribution."

      
runLowerUserCode :: (BasicVar b) => Fn b -> UserCode b -> CompM (Fn b)
runLowerUserCode = lowerUserCode
