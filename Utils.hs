module Utils where

import Data.Random.RVar
import Data.Random.Distribution.Categorical (weightedCategorical)

import Numeric.Log hiding (sum)
import Numeric.SpecFunctions (log1p, logBeta, logFactorial)

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

beta :: Double -> Double -> Log Double
beta a b = Exp $ logBeta a b

-- bernoulli_weight alpha beta
--
--      Log-domain weight of a Bernoulli success given nonzero
--      generalized counts of successes and failures, alpha and beta.
--      This is equivalent to
--
--              Exp $ log (alpha/(alpha + beta))
--
--      but computed more stably when beta <<< alpha:
--
--              log (alpha/(alpha + beta))
--                = -log ((alpha + beta)/alpha)
--                = -log (1 + beta/alpha)
--                = -log1p (beta/alpha)
--
bernoulli_weight :: Double -> Double -> Log Double
bernoulli_weight alpha beta = Exp $ -log1p (beta/alpha)
