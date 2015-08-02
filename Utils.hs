module Utils where

import Data.Random.RVar
import Data.Random.Distribution.Categorical (weightedCategorical)

flipweights :: [(a, Double)] -> RVar a
flipweights weights = weightedCategorical [(exp (p - maxp), idx) | (idx, p) <- weights]
    where maxp = maximum $ map snd weights

logsumexp :: [Double] -> Double
logsumexp xs = log $ sum $ adjusted where
    adjusted = map (\x -> x - max) xs
    max = maximum xs
