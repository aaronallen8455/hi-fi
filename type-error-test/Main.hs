import           System.Exit
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain . testCase "type errors" $
  (@?= ExitSuccess) =<< system "./type-error-test/run-test"
