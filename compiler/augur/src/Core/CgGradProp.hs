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

module Core.CgGradProp where

import Control.Monad.RWS
import qualified Data.Map as Map
    
import AstUtil.Var
import Core.DensSyn
import Comm.Prim
import qualified Low.LowSyn as L
import Low.LowXXSyn as LX
import Low.LowpPrimSyn
import Core.CoreSyn
import qualified Core.KernSyn as K
import Core.CoreTySyn
import Core.CgGrad
import Core.CgLike
import Compile.CompData
import qualified Low.LowShpSyn as S


----------------------------------------------------------------------
-- = CgGradProp Description
{-| [Note]

Generate gradient-based MCMC proposals.

-}


-----------------------------------
-- == Types and operations

type TyId = L.TyId


-----------------------------------
-- == Code-gen

cgGradProp :: CompInfo -> [TyId] -> [TyId] -> TyId -> TyId -> TyId -> TyId -> Name -> Name -> CompM (LX.LowPP TyId)
cgGradProp cinfo v_wrt vs_adj v_currBlk v_propBlk v_simLen v_stepSize gradName likeName =
    do let genSym = getGenSym cinfo
           ty = BlkTy (map getType' v_wrt)
       v_grad <- lift $ mkTyIdIO genSym (blkName' "blk_adj" v_wrt) ModAux ty
       v_mom0 <- lift $ mkTyIdIO genSym (blkName' "blk_mom0" v_wrt) ModAux ty
       v_mom <- lift $ mkTyIdIO genSym (blkName' "blk_mom" v_wrt)  ModAux ty
       let name = blkName' "hmc" v_wrt
           params = [ (v_simLen, RealTy), (v_stepSize, RealTy) ]
           args = [ L.Var v_currBlk
                  , L.Var v_propBlk
                  , L.Var v_grad
                  , L.Var v_mom0
                  , L.Var v_mom
                  , L.Var v_simLen
                  , L.Var v_stepSize ]
           e_leap = L.Call (L.PrimId DM_Fn PM_Fn (LeapFrog gradName likeName)) args
           allocs = [ v_grad, v_mom0, v_mom ]
           body = L.Exp e_leap
           decl = L.Fun name params allocs body Nothing UnitTy
           shpCtx = Map.fromList [ (v_grad, S.BlkOf vs_adj)
                                 , (v_mom0, S.BlkOf v_wrt)
                                 , (v_mom, S.BlkOf v_wrt) ]
       return $ LX.LowPP (LX.LowXX shpCtx True (LX.HostCall True) [] decl)

              
cgGradProp' :: CompInfo -> [TyId] -> [TyId] -> TyId -> TyId -> TyId -> TyId -> Name -> Name -> CompM (LX.LowPP TyId)
cgGradProp' cinfo v_wrt vs_adj v_currBlk v_propBlk v_simLen v_stepSize gradName likeName =
    do let genSym = getGenSym cinfo
           ty = BlkTy (map getType' v_wrt)
       v_grad <- lift $ mkTyIdIO genSym (blkName' "blk_adj" v_wrt) ModAux ty
       v_mom0 <- lift $ mkTyIdIO genSym (blkName' "blk_mom0" v_wrt) ModAux ty
       let name = blkName' "refl_slice" v_wrt
           params = [ (v_simLen, RealTy), (v_stepSize, RealTy) ]
           args = [ L.Var v_currBlk
                  , L.Var v_propBlk
                  , L.Var v_grad
                  , L.Var v_mom0
                  , L.Var v_simLen
                  , L.Var v_stepSize ]
           e_leap = L.Call (L.PrimId DM_Fn PM_Fn (ReflSlice gradName likeName)) args
           allocs = [ v_grad, v_mom0 ]
           body = L.Exp e_leap
           decl = L.Fun name params allocs body Nothing UnitTy
           shpCtx = Map.fromList [ (v_grad, S.BlkOf vs_adj)
                                 , (v_mom0, S.BlkOf v_wrt) ]
       return $ LX.LowPP (LX.LowXX shpCtx True (LX.HostCall True) [] decl)

              
cgGradProp'' :: CompInfo -> [TyId] -> [TyId] -> TyId -> TyId -> TyId -> Name -> Name -> CompM (LX.LowPP TyId)
cgGradProp'' cinfo v_wrt vs_adj v_currBlk v_propBlk v_stepSize gradName likeName =
    do let genSym = getGenSym cinfo
           ty = BlkTy (map getType' v_wrt)
       v_grad <- lift $ mkTyIdIO genSym (blkName' "blk_adj" v_wrt) ModAux ty
       v_work <- lift $ mkTyIdIO genSym (blkName' "blk_work" v_wrt) ModAux ty
       v_thetaMinus <- lift $ mkTyIdIO genSym (blkName' "blk_theta_minus" v_wrt) ModAux ty
       v_momMinus <- lift $ mkTyIdIO genSym (blkName' "blk_mom_minus" v_wrt) ModAux ty
       v_thetaPlus <- lift $ mkTyIdIO genSym (blkName' "blk_theta_plus" v_wrt) ModAux ty
       v_momPlus <- lift $ mkTyIdIO genSym (blkName' "blk_mom_plus" v_wrt) ModAux ty
       v_thetaShape <- lift $ mkTyIdIO genSym (blkName' "blk_theta_shape" v_wrt) ModAux ty
       let name = blkName' "nuts" v_wrt
           params = [ (v_stepSize, RealTy) ]
           args = [ L.Var v_currBlk
                  , L.Var v_propBlk
                  , L.Var v_grad
                  , L.Var v_work
                  , L.Var v_thetaMinus
                  , L.Var v_momMinus
                  , L.Var v_thetaPlus
                  , L.Var v_momPlus
                  , L.Var v_thetaShape
                  , L.Var v_stepSize ]
           e_leap = L.Call (L.PrimId DM_Fn PM_Fn (NUTS gradName likeName)) args
           allocs = [ v_grad, v_work, v_thetaMinus, v_momMinus, v_thetaPlus, v_momPlus, v_thetaShape ]
           body = L.Exp e_leap
           decl = L.Fun name params allocs body Nothing UnitTy
           shpCtx = Map.fromList [ (v_grad, S.BlkOf vs_adj)
                                 , (v_work, S.BlkOf v_wrt)
                                 , (v_thetaMinus, S.BlkOf v_wrt)
                                 , (v_momMinus, S.BlkOf v_wrt)
                                 , (v_thetaPlus, S.BlkOf v_wrt)
                                 , (v_momPlus, S.BlkOf v_wrt)
                                 , (v_thetaShape, S.BlkOf v_wrt) ]
       return $ LX.LowPP (LX.LowXX shpCtx True (LX.HostCall True) [] decl)

              
-----------------------------------
-- == Top-level
              
runHmcFn :: CompInfo -> CompOpt -> InferCtx TyId -> [TyId] -> TyId -> TyId -> Double -> Double -> Fn TyId -> CompM (K.Kern (LX.LowPP TyId) TyId)
runHmcFn cinfo copt inferCtx v_wrt v_currBlk v_propBlk simLen stepSize fn =
    do (shpCtx, vs_adj, grad) <- runGradFn cinfo copt inferCtx (mkName "FOOBAR") v_wrt fn
       like <- runLLFn cinfo copt inferCtx (blkName' "ll" v_wrt) fn
       v_simLen <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local RealTy -- TODO: hacked idkind, shoudl come up with better one
       v_stepSize <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local RealTy -- TODO: hacked idkind, shoudl come up with better one
       prop' <- cgGradProp cinfo v_wrt vs_adj v_currBlk v_propBlk v_simLen v_stepSize (L.declName grad) (L.declName like)       
       let like' = LX.LowPP (LX.LowXX Map.empty False (LX.HostCall False) [] like)
           grad' = LX.LowPP (LX.LowXX shpCtx False (LX.HostCall False) [] grad) -- TODO: move this to the gradient side
           kind' = K.GradProp (K.HMC grad' prop' simLen stepSize)
           allocs = map (\v -> (v, K.Reset)) (L.declAllocs (LX.getDecl (LX.unLowPP prop')))
           kernParams = [ v_simLen, v_stepSize ]
       return $ K.Base kind' (K.Block v_wrt) fn allocs kernParams like'

              
runRSliceFn :: CompInfo -> CompOpt -> InferCtx TyId -> [TyId] -> TyId -> TyId -> Double -> Double -> Fn TyId -> CompM (K.Kern (LX.LowPP TyId) TyId)
runRSliceFn cinfo copt inferCtx v_wrt v_currBlk v_propBlk simLen stepSize fn =
    do (shpCtx, vs_adj, grad) <- runGradFn cinfo copt inferCtx (mkName "FOOBAR") v_wrt fn
       like <- runLLFn cinfo copt inferCtx (blkName' "ll" v_wrt) fn
       v_simLen <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local RealTy -- TODO: hacked idkind, shoudl come up with better one
       v_stepSize <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local RealTy -- TODO: hacked idkind, shoudl come up with better one
       prop' <- cgGradProp' cinfo v_wrt vs_adj v_currBlk v_propBlk v_simLen v_stepSize (L.declName grad) (L.declName like)       
       let like' = LX.LowPP (LX.LowXX Map.empty False (LX.HostCall False) [] like)
           grad' = LX.LowPP (LX.LowXX shpCtx False (LX.HostCall False) [] grad) -- TODO: move this to the gradient side
           kind' = K.GradProp (K.Reflect grad' prop' simLen stepSize)
           allocs = map (\v -> (v, K.Reset)) (L.declAllocs (LX.getDecl (LX.unLowPP prop')))
           kernParams = [ v_simLen, v_stepSize ]
       return $ K.Base kind' (K.Block v_wrt) fn allocs kernParams like'


runNUTSFn :: CompInfo -> CompOpt -> InferCtx TyId -> [TyId] -> TyId -> TyId -> Double -> Fn TyId -> CompM (K.Kern (LX.LowPP TyId) TyId)
runNUTSFn cinfo copt inferCtx v_wrt v_currBlk v_propBlk stepSize fn =
    do (shpCtx, vs_adj, grad) <- runGradFn cinfo copt inferCtx (mkName "FOOBAR") v_wrt fn
       like <- runLLFn cinfo copt inferCtx (blkName' "ll" v_wrt) fn
       v_stepSize <- lift $ mkTyIdIO (getGenSym cinfo) Anon Local RealTy -- TODO: hacked idkind, shoudl come up with better one
       prop' <- cgGradProp'' cinfo v_wrt vs_adj v_currBlk v_propBlk v_stepSize (L.declName grad) (L.declName like)       
       let like' = LX.LowPP (LX.LowXX Map.empty False (LX.HostCall False) [] like)
           grad' = LX.LowPP (LX.LowXX shpCtx False (LX.HostCall False) [] grad) -- TODO: move this to the gradient side
           kind' = K.GradProp (K.NUTS grad' prop' stepSize)
           allocs = map (\v -> (v, K.Reset)) (L.declAllocs (LX.getDecl (LX.unLowPP prop')))
           kernParams = [ v_stepSize ]
       return $ K.Base kind' (K.Block v_wrt) fn allocs kernParams like'
