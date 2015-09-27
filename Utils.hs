module Utils where

import Data.Random.RVar
import Data.Random.Distribution.Categorical (weightedCategorical)

import Numeric.Log hiding (sum)
import Numeric.SpecFunctions (logFactorial)

flipweights :: [(a, Double)] -> RVar a
flipweights weights = weightedCategorical [(exp (p - maxp), idx) | (idx, p) <- weights]
    where maxp = maximum $ map snd weights

logsumexp :: [Double] -> Double
logsumexp xs = (log $ sum $ map exp adjusted) + max where
    adjusted = map (\x -> x - max) xs
    max = maximum xs

flipweights_ld :: [(a, Log Double)] -> RVar a
flipweights_ld weights = weightedCategorical [(ln $ exp (p / maxp), idx) | (idx, p) <- weights]
    where maxp = maximum $ map snd weights

choose :: Int -> Int -> Log Double
choose n k = Exp $ logFactorial n - logFactorial k - logFactorial (n-k)

log_domain :: (Floating a) => a -> Log a
log_domain = Exp . log
