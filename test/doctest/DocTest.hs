import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "symbolize" =<< getArgs
