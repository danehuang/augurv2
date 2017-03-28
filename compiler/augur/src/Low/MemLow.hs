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

{-# LANGUAGE FlexibleContexts #-}

module Low.MemLow where

import Control.Monad.RWS
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
    
import AstUtil.Fresh
import AstUtil.Pretty
import AstUtil.Var
import Compile.CompData
import Comm.DistSyn
import Comm.Prim
import Core.CoreSyn
import Low.LowSyn
import Core.CoreTySyn
import Low.ShpLowPP
import qualified Low.LowXXSyn as LX
import qualified Low.LowpPrimSyn as P
import qualified Low.LintLow as Lint
    
    
----------------------------------------------------------------------
-- = MemLow Description
{-| [Note]

Input: linearized + type-checked Lowp declaration
Output: unlinearized Lowpm declaration

def foobar(mu, cov) {
  x = MvNormal.sample(mu, cov)
}

def foobar(mu, cov) {
  lwork1 = AllocVecFromShape(mu)
  lwork2 = AllocMatFromShape(cov)
  x = AllocVecFromShape(mu)
  MvNormal.sample(x, mu, cov, lwork1, lwork2)
}

Size inference
lwork1 -> [sizeof(mu, 0)]
lwork2 -> [sizeof(cov, 0), sizeof(cov, 1)]

GLOBAL gmem1
GLOBAL gmem2

void foobar_prologue(mu, cov) {
  sz1 = [sizeof(mu, 0)]
  gmem1 = AllocMemFromSize(sz1)
  sz2 = [sizeof(cov, 0), sizeof(cov, 1)]
  gmem2 = AllocMemFromSize(sz2)
}
void foobar(mu, cov) {
  lwork1 = UseGlobalMemAsVec(gmem1, mu)
  lwork2 = UseGlobalMemAsMat(gmem2, cov)
  MvNormal.sample(x, mu, cov, lwork1, lwork2)
}
void foobar_epilogue(mu, cov) {
  sz1 = [sizeof(mu, 0)]
  FreeMemFromSize(gmem1, sz1)
  sz2 = [sizeof(cov, 0), sizeof(cov, 1)]
  FreeMemFromSize(gmem2, sz2)
}


def foobar'(mu, cov) {
  seq loop(i <- 0 until N) {
    seq loop(j <- 0 until M[i]) {
      store x [i, j] MvNormal(mu[i, j], cov[i, j])
    }
  }
}

def foobar'(mu, cov) {
  seq loop(i <- 0 until N) {
    seq loop(j <- 0 until M[i]) {
      lwork1 = AllocVecFromShape(mu[i, j])
      lwork2 = AllocMatFromShape(cov[i, j])
      MvNormal'(x[i, j], mu[i, j], cov[i, j], lwork1, lwork2)
    }
  }
}

foobar'_prologue(mu, cov) {
  sz1 = [sizeof(mu, 2)]
  gmem1 = AllocMemFromSize(sz1)
  sz2 = [sizeof(cov, 2), sizeof(cov, 3)]
  gmem2 = AllocMemFromSize(sz2)
}




def foobar''(mu, cov) {
  par loop(i <- 0 until N) {
    par loop(j <- 0 until M[i]) {
      store x [i, j] MvNormal(mu[i, j], cov[i, j])
    }
  }
}

def foobar''(mu, cov) {
  par loop(i <- 0 until N) {
    seq loop(j <- 0 until M[i]) {
      lwork1 = AllocVecFromShape(mu[i, j])
      lwork2 = AllocMatFromShape(cov[i, j])
      MvNormal'(x[i, j], mu[i, j], cov[i, j], lwork1, lwork2)
    }
  }
}

foobar''_prologue(mu, cov) {
  sz1 = [int(N), sizeof(mu, 2)]
  gmem1 = AllocMemFromSize(sz1)
  sz2 = [int(N), sizeof(cov, 2), sizeof(cov, 3)]
  gmem2 = AllocMemFromSize(sz2)
}

foobar''_epilogue(mu, cov) {
  sz1 = [int(N), sizeof(mu, 2)]
  FreeMemFromSize(gmem1, sz1)
  sz2 = [int(N), sizeof(cov, 2), sizeof(cov, 3)]
  FreeMemFromSize(gmem2, sz2)
}

-}

                        

-----------------------------------
-- == Types and operations

type LowerM b = RWST (LowerRdr b) [()] (LowerSt b) CompM
data LowerRdr b =
    LR { lr_loc :: Maybe (b, [Exp b])
       -- location that we are assigning or storing to
       , lr_atmInc :: Bool -- atmInc? 
       , lr_globTmpM :: Map.Map b [b]
       -- temporaries that we have already allocated to
       , lr_genSym :: GenSym
       }
data LowerSt b =
    LS { ls_alloc :: [b]
       , ls_stmts :: [Stmt b]
       }


liftLowerM :: IO a -> LowerM b a
liftLowerM c = lift $ lift $ c
       
freshId :: (TypedVar b Typ) => Name -> IdKind -> Typ -> LowerM b b
freshId name ik ty =
    do genSym <- asks lr_genSym
       liftLowerM $ mkTyIdIO genSym name ik ty
       
withPt :: (TypedVar b Typ) => b -> [Exp b] -> LowerM b (Exp b) -> LowerM b (Exp b)
withPt x idxs = local (\rdr -> rdr { lr_loc = Just (x, idxs) })

withAtmInc :: Bool -> LowerM b a -> LowerM b a
withAtmInc atmInc = local (\rdr -> rdr { lr_atmInc = atmInc })
                
emitStmt :: (TypedVar b Typ) => Stmt b -> LowerM b ()
emitStmt s = modify (\st -> st { ls_stmts = s : ls_stmts st })

clearStmts :: (TypedVar b Typ) => LowerM b ()
clearStmts = modify (\st -> st { ls_stmts = [] })

getStmts :: (TypedVar b Typ) => LowerM b [Stmt b]
getStmts = gets ls_stmts
             
alloc :: (TypedVar b Typ) => b -> LowerM b ()
alloc x = modify (\st -> st { ls_alloc = x : ls_alloc st })

allocVec' :: (TypedVar b Typ) => Exp b -> Typ ->  LowerM b (Exp b)
allocVec' e_shp ty =
    do v <- freshId Anon ModAux ty
       alloc v
       return $ Call (PrimId DM_Mem PM_Fn P.ReadVecFromShape) [ Var v, e_shp ]

allocMat' :: (TypedVar b Typ) => Exp b -> Typ -> LowerM b (Exp b)
allocMat' e_shp ty =
    do v <- freshId Anon ModAux ty
       alloc v
       return $ Call (PrimId DM_Mem PM_Fn P.ReadMatFromShape) [ Var v, e_shp ]

                       
-----------------------------------
-- == Transformation

getLoc :: (TypedVar b Typ) => LowerM b (b, [Exp b])
getLoc = asks lr_loc >>= return . fromJust


-- TODO: Refactor me
lowerMvNormal :: (TypedVar b Typ) => Dop -> [Exp b] -> LowerM b (Exp b)
lowerMvNormal dop es' =
    case dop of
      LL ->
          do e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocVec' (es' !! 1) (VecTy RealTy)
             let es'' = es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem MvNormal es''
      Pdf ->
          do e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocVec' (es' !! 1) (VecTy RealTy)
             let es'' = es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem MvNormal es''
      Sample ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 1) (MatTy RealTy)
             e_work <- allocVec' (es' !! 0) (VecTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem MvNormal es''
      (Conj _ _) ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 1) (MatTy RealTy)
             e_work <- allocVec' (es' !! 0) (VecTy RealTy)
             e_llt' <- allocMat' (es' !! 1) (MatTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work, e_llt' ]
             return $ DistOp dop DM_Mem MvNormal es''
      DotPt ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocVec' (es' !! 1) (VecTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem MvNormal es''
      DotArg1 ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocVec' (es' !! 1) (VecTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem MvNormal es''
      DotArg2 ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocVec' (es' !! 1) (VecTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem MvNormal es''


-- TODO: Refactor me
lowerInvWishart :: (TypedVar b Typ) => Dop -> [Exp b] -> LowerM b (Exp b)
lowerInvWishart dop es' =
    case dop of
      LL ->
          do e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocMat' (es' !! 2) (MatTy RealTy)
             let es'' = es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem InvWishart es''
      Pdf ->
          do e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocMat' (es' !! 2) (MatTy RealTy)
             let es'' = es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem InvWishart es''
      Sample ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 1) (MatTy RealTy)
             e_work <- allocMat' (es' !! 1) (MatTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem InvWishart es''
      (Conj _ _) ->
          -- TODO: HACK
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 1) (MatTy RealTy)
             e_work <- allocMat' (es' !! 1) (MatTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
                 -- es'' = mkDensPt x idxs : es'
             return $ DistOp dop DM_Mem InvWishart es''
      DotPt ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocMat' (es' !! 2) (MatTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem InvWishart es''
      DotArg1 ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocMat' (es' !! 2) (MatTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem InvWishart es''
      DotArg2 ->
          do (x, idxs) <- getLoc
             e_llt <- allocMat' (es' !! 2) (MatTy RealTy)
             e_work <- allocMat' (es' !! 2) (MatTy RealTy)
             let es'' = mkDensPt x idxs : es' ++ [ e_llt, e_work ]
             return $ DistOp dop DM_Mem InvWishart es''


                    
lowerDist :: (TypedVar b Typ) => Dop -> DopMode -> Dist -> [Exp b] -> LowerM b (Exp b)
lowerDist dop dm dist es =
    do when (dm == DM_Mem) (throwError $ "@lowerExp | Expected only DM_Fn operations at this point: " ++ pprShow (DistOp dop dm dist es))
       es' <- mapM lowerExp es
       case dist of
         Dirichlet ->
             if hasPt dop
             then return $ DistOp dop DM_Mem Dirichlet es'
             else
                 do (x, idxs) <- getLoc
                    return $ DistOp dop DM_Mem Dirichlet (mkDensPt x idxs : es')
         MvNormal -> lowerMvNormal dop es'
         InvWishart -> lowerInvWishart dop es'
         _ -> return $ DistOp dop DM_Mem dist es'


typOf :: (TypedVar b Typ) => Exp b -> Typ
typOf (Var x) = fromJust (getType x)
typOf (Lit lit) =
    case lit of
      Int _ -> IntTy
      Real _ -> RealTy
typOf e = error $ "[MemLow] | foobar: " ++ pprShow e
            
-- For every new primitive, need to be able to compute memory from its arguments
lowerPrim :: (TypedVar b Typ) => [Exp b] -> P.Prim -> PrimMode -> LowerM b (Exp b)
lowerPrim es' prim pm =
    case prim of
      P.DotProd ->
          case pm of
            PM_Fn -> return $ Call (PrimId DM_Mem pm P.DotProd) es'
            PM_Grad _ ->
                do e_work <- allocVec' (es' !! 1) (VecTy RealTy)
                   let es'' = e_work : es'
                   return $ Call (PrimId DM_Mem pm P.DotProd) es''
      P.AllocVecFromShape ->
          -- TODO: HACK??
          return $ Call (PrimId DM_Mem pm P.AllocVecFromShape) es'
      P.ReadVecFromShape ->
          -- TODO: HACK??
          allocVec' (es' !! 0) (typOf (es' !! 0))
          -- return $ Call (PrimId M.AllocVecFromShape pm) es'
      P.AllocMatFromShape ->
          -- TODO: HACK??
          return $ Call (PrimId DM_Mem pm P.AllocMatFromShape) es'
      P.ReadMatFromShape ->
          -- TODO: HACK??
          allocMat' (es' !! 0) (typOf (es' !! 0))
          -- return $ Call (PrimId M.AllocMatFromShape pm) es'
      P.MWG name1 name2 name3 ->
          return $ Call (PrimId DM_Mem pm (P.MWG name1 name2 name3)) es'
      P.EllipSlice name ->
          return $ Call (PrimId DM_Mem pm (P.EllipSlice name)) es'
      P.LeapFrog gradName likeName ->
          return $ Call (PrimId DM_Mem pm (P.LeapFrog gradName likeName)) es'
      _ -> return $ Call (PrimId DM_Mem pm prim) es'
                           
                     
            
lowerExp :: (TypedVar b Typ) => Exp b -> LowerM b (Exp b)
lowerExp (Var x) =
    return $ Var x
lowerExp (Lit lit) =
    return $ Lit lit
lowerExp (DistOp dop dm dist es) =
    lowerDist dop dm dist es                      
lowerExp (Call ce es) =
    do es' <- mapM lowerExp es
       case ce of
         FnId name -> return $ Call (FnId name) es'
         PrimId _ pm prim -> lowerPrim es' prim pm
lowerExp (Proj e es) =
    do es' <- mapM lowerExp es
       e' <- lowerExp e
       return $ Proj e' es'

              
lowerGen :: (TypedVar b Typ) => Gen b -> LowerM b (Gen b)
lowerGen (Until e1 e2) =
    do e1' <- lowerExp e1
       e2' <- lowerExp e2
       return $ Until e1' e2'
              
ukToAtmInc :: UpKind -> Bool
ukToAtmInc AtmInc = True
ukToAtmInc _ = False

              
lowerStmt :: (TypedVar b Typ) => Stmt b -> LowerM b (Stmt b)
lowerStmt Skip =
    return Skip
lowerStmt (Exp e) =
    do clearStmts
       e' <- lowerExp e
       s <- getStmts
       return $ seqStmt (reverse (Exp e' : s))
lowerStmt (Assign x e) =
    do clearStmts
       e' <- withPt x [] (lowerExp e)
       stmts <- getStmts
       s <- case e' of
              DistOp _ _ Dirac _ ->
                  return $ Assign x e'
              DistOp dop _ dist _ ->
                  if isMv dist
                  then
                      case dop of
                        LL -> return $ Assign x e'
                        Pdf -> return $ Assign x e'
                        _ -> return $ Exp e'
                  else return $ Assign x e'
              Call (PrimId _ pm prim) _ ->
                  case (prim, pm) of
                    (P.DotProd, PM_Grad _) -> return $ Exp e'
                    _ -> return $ Assign x e'
              _ -> return $ Assign x e'
       return $ seqStmt (reverse (s : stmts))
lowerStmt (Store x es uk eval) =
    do clearStmts
       es' <- mapM lowerExp es
       eval' <- withAtmInc (ukToAtmInc uk) (withPt x es' (lowerExp eval))
       stmts <- getStmts
       s <- case eval' of
              DistOp _ _ Dirac _ ->
                  return $ Store x es' uk eval'
              DistOp dop _ dist _ ->
                  if isMv dist
                  then
                      case dop of
                        LL -> return $ Store x es' uk eval'
                        Pdf -> return $ Store x es' uk eval'
                        _ -> return $ Exp eval'
                  else return $ Store x es' uk eval'
              Call (PrimId _ pm prim) _ ->
                  case (prim, pm) of
                    (P.DotProd, PM_Grad _) -> return $ Exp eval'
                    _ -> return $ Store x es' uk eval'
              _ -> return $ Store x es' uk eval'
       return $ seqStmt (reverse (s : stmts))
lowerStmt (Seq s1 s2) =
    do s1' <- lowerStmt s1
       s2' <- lowerStmt s2
       return $ Seq s1' s2'
lowerStmt (If e s1 s2) =
    do e' <- lowerExp e
       s1' <- lowerStmt s1
       s2' <- lowerStmt s2
       return $ If e' s1' s2'
lowerStmt (Loop lk x gen s) =
    do clearStmts
       gen' <- lowerGen gen
       stmts <- getStmts
       s' <- lowerStmt s
       return $ seqStmt (reverse (Loop lk x gen' s' : stmts))
lowerStmt (MapRed acc x gen s e) =
    do clearStmts
       gen' <- lowerGen gen
       stmts1 <- getStmts
       s' <- lowerStmt s
       clearStmts
       e' <- lowerExp e
       stmts2 <- getStmts
       return $ seqStmt (reverse (MapRed acc x gen' (seqStmt (s' : reverse stmts2)) e' : stmts1))

              
lowerDecl :: (TypedVar b Typ) => LX.LowPP b -> LowerM b (Decl b)
lowerDecl (LX.LowPP (LX.LowXX shpCtx _ cc _ (Fun name params allocs body retExp retTy))) =
    do body' <- lowerStmt body
       clearStmts
       retExp' <- case retExp of
                    Just e -> lowerExp e >>= return . Just
                    Nothing -> return Nothing
       stmts <- getStmts
       allocs' <- gets ls_alloc
       let decl' = Fun name params (allocs ++ allocs') (seqStmt (body' : reverse stmts)) retExp' retTy
       return decl'

              
-----------------------------------
-- == Top-level

runLowerDecl :: (TypedVar b Typ) => CompInfo -> CompOpt -> InferCtx b -> LX.LowPP b -> CompM (LX.LowMM b)
runLowerDecl cinfo copt inferCtx lowppDecl =
    do debugM "Low.MemLow" $ "LowPP (Input):\n" ++ pprShow lowppDecl
       (decl', _, _) <- runRWST (lowerDecl lowppDecl) (LR Nothing False Map.empty (getGenSym cinfo)) (LS [] [])
       debugM "Low.MemLow" $ "LowMM (Intermediate):\n" ++ pprShow decl'
       decl'' <- runLint copt decl' (Lint.runLintDecl cinfo False inferCtx)
       shpCtx' <- runShpiDecl inferCtx (LX.getGlobs (LX.unLowPP lowppDecl)) decl''
       let useAux = LX.useMcmcPropSt (LX.unLowPP lowppDecl)
           cc = LX.getCC (LX.unLowPP lowppDecl)
           projIdx = LX.projIdx (LX.unLowPP lowppDecl)
           lowmmDecl = LX.LowMM (LX.LowXX shpCtx' useAux cc projIdx decl'')
       debugM "Low.MemLow" $ "LowMM (Output):\n" ++ pprShow lowmmDecl
       return $ lowmmDecl
          
