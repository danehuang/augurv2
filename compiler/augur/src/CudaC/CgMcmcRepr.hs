{- 
 - Copyright 2017 Daniel Eachern Huang
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at

 -    http://www.apache.org/licenses/LICENSE-2.0

 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module CudaC.CgMcmcRepr
    (runCgHdr) where

import qualified Data.Map as Map
import Control.Monad.Reader
import Debug.Trace
    
import AstUtil.Pretty
import AstUtil.Var
import AstUtil.Fresh
import Compile.CompData
import Core.KernSyn
import Core.CoreSyn
import Core.CoreTySyn
import Low.LowShpSyn as S
import qualified CudaC.CgCudaCCore as CCG
import qualified CudaC.CudaCSyn as C



----------------------------------------------------------------------
-- = CgMcmcRepr Description
{-| [Note]

Generates native MCMC data structure.

aux {
  hyper_1 ; ... ; hyper_n ;
  alloc_1 ; ... ; alloc_m ;
}

model {
  param_1 ; ... ; param_n ;
  blk_1 ; ... ; blk_m ;       (for blocked variables)
  data_1 ; ... ; data_l ;
}

-}



-----------------------------------
-- == Types and operations

type TyId = TVar Typ
type CTyId = TVar C.Typ
type ReprM = ReaderT GenSym CompM


cgId :: TyId -> CTyId
cgId = CCG.cgIdLhs
            
    
-----------------------------------
-- == Transformations

isPromote :: IdKind -> Bool
isPromote (ModParam _) = True
isPromote ModParamDup = True
isPromote ModData = True
isPromote ModAux = True
isPromote _ = False

cgStrctDecl :: TyId -> (CTyId, C.Typ)
cgStrctDecl v = (cgId v, ty)
    where
      ty = if isPromote (idKind v)
           then
               case getType' v of
                 IntTy -> C.PtrTy C.IntTy
                 RealTy -> C.PtrTy C.DblTy
                 ty' -> CCG.cgTypTop ty'
           else CCG.cgTypTop (getType' v)


-- Invariant: scalar types should already be promoted at this point?
-- Invariant: allocations mentioned in a kernel are in the shpCtx
cgAuxStruct :: InferCtx TyId -> S.ShpCtx TyId -> [TyId] -> TVar C.Typ -> ReprM (C.Decl CTyId)
cgAuxStruct inferCtx shpCtx kernParams v_rng =
    do let rng = (v_rng, getType' v_rng)
           modDecls = ic_modDecls inferCtx
           hypers = map cgStrctDecl (getModHyperIds modDecls)
           allocs = map cgStrctDecl (map fst (Map.toList shpCtx))
           kernParams' = map cgStrctDecl kernParams
       traceM $ "[CgMcmcRepr] | modDecls " ++ show (getModDeclIds modDecls)
       return $ C.Struct "AugurAux" (rng : hypers ++ allocs ++ kernParams') (Just "AugurAux_t")
       
             
cgModStruct :: InferCtx TyId -> Kern c TyId -> ReprM (C.Decl CTyId)
cgModStruct inferCtx kern =
    do let modDecls = ic_modDecls inferCtx
           modParams = getModParamIds modDecls
           modData = getModDataIds modDecls
           blkDecls = [] -- concat $ map addBlkDecls ks
       return $ C.Struct "AugurMod" (map cgStrctDecl (modParams ++ blkDecls ++ modData)) (Just "AugurMod_t")


cgHdr :: InferCtx TyId -> Kern c TyId -> S.ShpCtx TyId -> TVar C.Typ -> ReprM String
cgHdr inferCtx kern shpCtx v_rng =
    do sAux <- cgAuxStruct inferCtx shpCtx (gatherKernParams kern) v_rng >>= return . pprShow
       sModel <- cgModStruct inferCtx kern >>= return . pprShow      
       return $ sAux ++ "\n" ++ sModel

              
runCgHdr :: CompInfo -> InferCtx TyId -> Kern c TyId -> S.ShpCtx TyId -> TVar C.Typ -> CompM String
runCgHdr cinfo inferCtx kern shpCtx v_rng =
    runReaderT (cgHdr inferCtx kern shpCtx v_rng) (getGenSym cinfo)

