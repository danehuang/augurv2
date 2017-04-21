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

module CudaC.XfaceCudaC where
    
import AstUtil.Pretty
import AstUtil.Var
import Comm.DistSyn
import Comm.Prim
import qualified CudaC.CudaCSyn as C
import Low.LowSyn
import Core.CoreTySyn
import Low.LowpPrimSyn
import Compile.CompData


----------------------------------------------------------------------
-- = XfaceCudaC Description
{-| [Note]

Contains bindings for Cuda/C.

-}

compErr :: String -> a
compErr msg = compErrMod "XfaceCudaC" msg
                 
-----------------------------------
-- == Types

ty_AugurTyp :: C.Typ
ty_AugurTyp = C.NameTy "AugurTyp_t"

ty_AugurAux :: C.Typ
ty_AugurAux = C.NameTy "AugurAux_t"
             
ty_AugurMod :: C.Typ
ty_AugurMod = C.NameTy "AugurMod_t"

ty_AugurVec :: C.Typ
ty_AugurVec = C.NameTy "AugurVec_t"

ty_AugurFlatVec :: C.Typ
ty_AugurFlatVec = C.NameTy "AugurFlatVec_t"
             
ty_AugurMat :: C.Typ
ty_AugurMat = C.NameTy "AugurMat_t"
             
ty_AugurBlk :: C.Typ
ty_AugurBlk = C.NameTy "AugurBlk_t"

ty_AugurDim :: C.Typ
ty_AugurDim = C.NameTy "AugurDim_t"

ty_AugurShape :: C.Typ
ty_AugurShape = C.NameTy "AugurShape_t"
                
                
-----------------------------------
-- == Constants

enum_AUGUR_CPU :: String
enum_AUGUR_CPU = "AUGUR_CPU"
                 
exp_AUGUR_CPU :: C.Exp (TVar C.Typ)
exp_AUGUR_CPU = C.mkLiterally enum_AUGUR_CPU

enum_AUGUR_GPU :: String
enum_AUGUR_GPU = "AUGUR_GPU"
                
exp_AUGUR_GPU :: C.Exp (TVar C.Typ)
exp_AUGUR_GPU = C.mkLiterally enum_AUGUR_GPU

targetToExp :: Target -> C.Exp (TVar C.Typ)
targetToExp CPU = exp_AUGUR_CPU
targetToExp (GPU _) = exp_AUGUR_GPU
                
enum_AUGUR_INT :: String
enum_AUGUR_INT = "AUGUR_INT"

exp_AUGUR_INT :: C.Exp (TVar C.Typ)
exp_AUGUR_INT = C.mkLiterally enum_AUGUR_INT
                 
enum_AUGUR_DBL :: String
enum_AUGUR_DBL = "AUGUR_DBL"

exp_AUGUR_DBL :: C.Exp (TVar C.Typ)
exp_AUGUR_DBL = C.mkLiterally enum_AUGUR_DBL
                 
enum_AUGUR_VEC :: String
enum_AUGUR_VEC = "AUGUR_VEC"

enum_AUGUR_MAT :: String
enum_AUGUR_MAT = "AUGUR_MAT"

enum_AUGUR_BLK :: String
enum_AUGUR_BLK = "AUGUR_BLK"

enum_TRUE :: String
enum_TRUE = "TRUE"

exp_TRUE :: C.Exp (TVar C.Typ)
exp_TRUE = C.mkLiterally enum_TRUE

          
ty2AugurTy :: Typ -> String
ty2AugurTy IntTy = enum_AUGUR_INT
ty2AugurTy RealTy = enum_AUGUR_DBL
ty2AugurTy (VecTy _) = enum_AUGUR_VEC
ty2AugurTy (MatTy _) = enum_AUGUR_MAT
ty2AugurTy (BlkTy _) = enum_AUGUR_BLK
ty2AugurTy _ = error $ "[CodeGenC] @ty2AugurTy | Shouldn't happen"

               
-----------------------------------
-- == Common expressions 

exp_MCMC :: C.Exp (TVar C.Typ)
exp_MCMC = C.mkLiterally "MCMC"

exp_MCMC_AUX :: C.Exp (TVar C.Typ)
exp_MCMC_AUX = C.strctProj' exp_MCMC "aux"

exp_MCMC_CURR :: C.Exp (TVar C.Typ)
exp_MCMC_CURR = C.strctProj' exp_MCMC "curr"

exp_MCMC_PROP :: C.Exp (TVar C.Typ)
exp_MCMC_PROP = C.strctProj' exp_MCMC "prop"
                
projMCMC :: String -> C.Exp (TVar C.Typ)
projMCMC = C.strctProj' exp_MCMC

           
-----------------------------------
-- == Cuda expressions

exp_BLOCKIDX :: C.Exp (TVar C.Typ)
exp_BLOCKIDX = C.mkLiterally "blockIdx"

exp_BLOCKIDX_X :: C.Exp (TVar C.Typ)
exp_BLOCKIDX_X = C.Binop exp_BLOCKIDX C.Proj (C.mkLiterally "x")
               
exp_BLOCKDIM :: C.Exp (TVar C.Typ)
exp_BLOCKDIM = C.mkLiterally "blockDim"

exp_BLOCKDIM_X :: C.Exp (TVar C.Typ)
exp_BLOCKDIM_X = C.Binop exp_BLOCKDIM C.Proj (C.mkLiterally "x")
               
exp_THREADIDX :: C.Exp (TVar C.Typ)
exp_THREADIDX = C.mkLiterally "threadIdx"

exp_THREADIDX_X :: C.Exp (TVar C.Typ)
exp_THREADIDX_X = C.Binop exp_THREADIDX C.Proj (C.mkLiterally "x")
                
               
-----------------------------------
-- == Library functions

emitPrim :: Prim -> String
emitPrim Plus = "augur_plus"
emitPrim Minus = "augur_minus"
emitPrim Times = "augur_times"
emitPrim Div = "augur_div"
emitPrim EqEq = "augur_eqeq"
emitPrim Neg = "augur_neg"
emitPrim Expon = "augur_exp"
emitPrim Log = "augur_log"
emitPrim Expit = "augur_expit"
emitPrim Logit = "augur_logit"
emitPrim MinusVec = "augur_vec_minus"
emitPrim SizeVec = "AUGUR_VEC_ELEMS"
emitPrim NormVec = "augur_vec_norm"
emitPrim DotProd = "augur_dotprod"
emitPrim AllocVecFromShape = "allocVecFromShape"
emitPrim ReadVecFromShape = "readVecFromShape"
emitPrim AllocMatFromShape = "allocMatFromShape"
emitPrim ReadMatFromShape = "readMatFromShape"
emitPrim NormAndSamp = "augur_unnorm_disc_samp"
emitPrim AtmIncVec = "augur_vec_atm_plus"
emitPrim AtmIncMatVTMT = "augur_mat_atm_inc_vtmt"
emitPrim PllSumVec = "augur_pll_sum_vec"
emitPrim (EllipSlice _) = "augur_mcmc_eslice"
emitPrim (LeapFrog _ _) = "h_augur_mcmc_hmc"
emitPrim prim = compErr $ "Cannot emit primitive " ++ pprShow prim

            
getPrimLibFn :: PrimMode -> Prim -> String
getPrimLibFn pm prim =
    case pm of
      PM_Fn -> emitPrim prim
      PM_Grad pos -> emitPrim prim ++ "_grad_" ++ show pos
         

-----------------------------------
-- == Distributions
                   
emitDist :: Dist -> String
emitDist Dirac = compErr $ "Expected distribution but found " ++ pprShow Dirac
emitDist Bernoulli = "bernoulli"
emitDist Categorical = "categorical"
emitDist Geometric = "geometric"
emitDist Poisson = "poisson"
emitDist Beta = "beta"
emitDist Exponential = "exponential"
emitDist Gamma = "gamma"
emitDist InvGamma = "invgamma"
emitDist Normal = "normal"
emitDist Uniform = "uniform"
emitDist Dirichlet = "dirichlet"
emitDist MvNormal = "mvnormal"
emitDist InvWishart = "invwishart"

                      
emitDop :: Dop -> String
emitDop LL = "ll"
emitDop Pdf = "pdf"
emitDop Sample = "sample"
emitDop (Conj dist _) = emitDist dist ++ "_" ++ "conj"
emitDop DotPt = "dotpt"
emitDop DotArg1 = "dotarg1"
emitDop DotArg2 = "dotarg2"

                  
getDistLibFn :: Dop -> Dist -> String
getDistLibFn dop dist =
    "augur" ++ "_" ++ emitDist dist ++ "_" ++ emitDop dop


            
-----------------------------------
-- == Projecting

getProjLibFn :: Typ -> Int -> String
getProjLibFn ty len =
    case ty of
      VecTy IntTy -> "AUGUR_VEC_GETI"
      VecTy RealTy -> "AUGUR_VEC_GETD"
      VecTy (VecTy _) -> "AUGUR_VEC_GETV"
      VecTy (MatTy _) -> "AUGUR_VEC_GETM"
      MatTy ty' ->
          case len of
            1 -> "getMatv" -- TODO!?
            2 -> case ty' of
                   IntTy -> "getMati"  -- TODO!?
                   RealTy -> "getMatd" -- TODO!?
                   _ -> compErr $ "Expected base type but found " ++ pprShow ty'
            _ -> compErr $ "Length should be less than two but found length " ++ pprShow len
      _ -> compErr $ "Expected projectable type but found " ++ pprShow ty
          

           
-----------------------------------
-- == Storing
{-| [Note]

Suppose v has base type ty. Then:

v : ModParam _   -> ty*
v : ModParamDup  -> ty*
v : ModData      -> ty*
v : ModAux       -> ty*
v : _            -> ty

-}

isIndirect :: IdKind -> Bool
isIndirect (ModParam _) = True
isIndirect ModParamDup = True
isIndirect ModData = True -- TODO?: Interesting, doesn't need to be this way
isIndirect ModAux = True
isIndirect _ = False

               
prmtStoreDst :: IdKind -> UpKind -> Typ -> Bool
prmtStoreDst ik uk ty =
    case uk of
      Update -> False
      AtmInc ->
          case ty of
            IntTy -> not (isIndirect ik)
            RealTy -> not (isIndirect ik)
            _ -> False
      Inc -> False
      AtmMInc ->
          case ty of
            IntTy -> not (isIndirect ik)
            RealTy -> not (isIndirect ik)
            _ -> False
      MInc -> False

           
getStoreLibFn :: IdKind -> UpKind -> Typ -> Typ -> Either String C.Aop
getStoreLibFn ik uk objTy storeTy =    
    case uk of
      Update ->
          case (objTy, storeTy) of
            (IntTy, IntTy) -> if isIndirect ik then Left "AUGUR_SETI" else Right C.EqAss
            (RealTy, RealTy) -> if isIndirect ik then Left "AUGUR_SETD" else Right C.EqAss
            (VecTy IntTy, IntTy) -> Left "AUGUR_VEC_SETI"
            (VecTy RealTy, RealTy) -> Left "AUGUR_VEC_SETD"
            (VecTy (VecTy _), VecTy _) -> Left "AUGUR_VEC_SETV"
            (VecTy _, VecTy _) -> Left "augur_vec_cpy"
            (VecTy (MatTy _), MatTy _) -> Left "AUGUR_VEC_SETM"
            (MatTy _, MatTy _) -> Left "augur_mat_cpy"
            _ -> err
      AtmInc ->
          case (objTy, storeTy) of
            (IntTy, IntTy) -> Left "AUGUR_ATMINCI"
            (RealTy, RealTy) -> Left "AUGUR_ATMINCD"
            (VecTy IntTy, IntTy) -> Left "AUGUR_VEC_ATMINCI"
            (VecTy RealTy, RealTy) -> Left "AUGUR_VEC_ATMINCD"
            (VecTy IntTy, VecTy IntTy) -> Left "augur_vec_atm_plus"
            (VecTy RealTy, VecTy RealTy) -> Left "augur_vec_atm_plus"
            (MatTy IntTy, MatTy IntTy) -> Left "augur_vec_atm_plus"
            (MatTy RealTy, MatTy RealTy) -> Left "augur_mat_atm_plus"          
            _ -> err
      Inc ->
          case objTy of
            IntTy -> if isIndirect ik then Left "AUGUR_INCI" else Right C.PlusEqAss
            RealTy -> if isIndirect ik then Left "AUGUR_INCD" else Right C.PlusEqAss
            VecTy IntTy -> Left "AUGUR_VEC_INCI"
            VecTy RealTy -> Left "AUGUR_VEC_INCD"
            _ -> err
      AtmMInc ->
          case objTy of
            IntTy -> Left "AUGUR_ATMMINCI"
            RealTy -> Left "AUGUR_ATMMINCD"
            VecTy IntTy -> Left "AUGUR_VEC_ATMMINCI"
            VecTy RealTy -> Left "AUGUR_VEC_ATMMINCD"
            _ -> err
      MInc ->
          case objTy of
            IntTy -> if isIndirect ik then Left "AUGUR_MINCI" else Right C.TimesEqAss
            RealTy -> if isIndirect ik then Left "AUGUR_MINCD" else Right C.TimesEqAss
            VecTy IntTy -> Left "AUGUR_VEC_MINCI"
            VecTy RealTy -> Left "AUGUR_VEC_MINCD"
            _ -> err
    where
      err = compErr $ "Cannot codegen update kind " ++ pprShow uk ++ " with " ++ pprShow ik ++ ", " ++ pprShow objTy ++ ", " ++ pprShow storeTy

            
                
