import System.Exit
import Test.HUnit

tests = test [assertFailure "Boo!"]

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT $ tests
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
