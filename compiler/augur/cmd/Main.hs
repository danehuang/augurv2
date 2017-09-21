import Data.Either
import Control.Monad.Except
    
import AstUtil.Fresh
import AstUtil.Pretty    
import qualified Core.ParseCore
import Low.ParseLowPP
import Low.LowSyn
import Core.CoreSyn
import Core.DensSyn
import Core.LintCore
-- import Low.LowpPrimSyn
import Low.RnLow
-- import Low.CgLowpm
import qualified Core.RnCore
import Compile.Compile
import Compile.CompData
import Rv.ParseRv
import Rv.LowerRv


type TestM = ExceptT String IO    

testCompile :: String -> Maybe String -> [Int] -> Target -> IO ()
testCompile model Nothing rtSizes target =
    do fModel <- readFile model
       case (Rv.ParseRv.runParseModel fModel) of
         Right model ->
             do v <- runComp (compile model Nothing rtSizes target)
                case v of
                  Left errMsg -> putStrLn $ "ERROR: " ++ errMsg
                  Right (pyModParam, pyTys, inferHdr, inferCode) ->
                      putStrLn $ pprShow inferCode
testCompile model (Just infer) rtSizes target =
    do fModel <- readFile model
       fInfer <- readFile infer
       case (Rv.ParseRv.runParseModel fModel, Rv.ParseRv.runParseKer fInfer) of
         (Right model, Right infer) ->
             do v <- runComp (compile model (Just infer) rtSizes target)
                case v of
                  Left errMsg -> putStrLn $ "ERROR: " ++ errMsg
                  Right (pyModParam, pyTys, inferHdr, inferCode) ->
                      putStrLn $ pprShow inferCode
         (Left errMsg, _) -> putStrLn $ "Error parsing model: " ++ errMsg
         (_, Left errMsg) -> putStrLn $ "Error parsing inference: " ++ errMsg

           
main :: IO ()
main =
    do let target = GPU BlkStrat
           -- target = CPU
       -- [ K, N, lam, x ]
       -- testCompile "test/hlr.rv" (Just "test/hlr1.infer") [ 10, 1000, 1, -1 ] target
       -- testCompile "test/hlr.rv" (Just "test/hlr2.infer") [ 10, 1000, 1, -1, -1, -1, 10, 1000 ] target

       -- testCompile "test/dethlr.rv" Nothing
       -- testCompile "test/dethlr.rv" (Just "test/hlr1.infer") target
       -- testCompile "test/dethlr.rv" (Just "test/hlr2.infer") target

       -- testCompile "test/mvgmm.rv" Nothing
       -- testCompile "test/mvgmm.rv" (Just "test/mvgmm1.infer") [] target
       -- testCompile "test/mvgmm.rv" (Just "test/mvgmm2.infer") [] target
       -- testCompile "test/mvgmm.rv" (Just "test/mvgmm3.infer") [] target
       -- testCompile "test/mvgmm.rv" (Just "test/mvgmm4.infer") [] target
       -- testCompile "test/mvgmm.rv" (Just "test/mvgmm5.infer") [] target
       -- testCompile "test/mvgmm.rv" (Just "test/mvgmm6.infer") [] target
                          

       -- testCompile "test/hmvgmm.rv" (Just "test/hmvgmm1.infer") [] target
       -- testCompile "test/hmvgmm.rv" (Just "test/hmvgmm2.infer") target
       -- testCompile "test/hmvgmm.rv" (Just "test/hmvgmm3.infer") target
       
       -- K = 3, D = 4, N = -1, sizeof(alpha) = 3, sizeof(beta) = 20
       testCompile "test/lda.rv" (Just "test/lda1.infer") [3, 4, -1, 3, 20] target
