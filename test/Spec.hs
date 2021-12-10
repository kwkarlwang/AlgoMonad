import Test.HUnit (Test (TestList), runTestTT)
import TestProblem as TP
import TestProblemDetail as TPD
import TestUtils as TU

main :: IO ()
main = do
  runTestTT $ TestList $ TP.tests ++ TPD.tests ++ TU.tests
  return ()
