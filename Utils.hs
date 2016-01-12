-- -*- coding: utf-8 -*-

--   Copyright (c) 2010-2014, MIT Probabilistic Computing Project
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

module Utils where

import Data.Maybe (fromMaybe)
import Data.Random.RVar
import Data.Random.Distribution.Categorical (weightedCategorical)

import Numeric.Log hiding (log1p, sum)
import Numeric.SpecFunctions (log1p, logBeta, logGamma, logFactorial)

flipweights :: [(a, Double)] -> RVar a
flipweights weights = weightedCategorical weights'
    where weights' = [(exp (p - maxp), idx) | (idx, p) <- weights]
          maxp = maximum $ map snd weights

logsumexp :: [Double] -> Double
logsumexp xs = (log $ sum $ map exp adjusted) + max where
    adjusted = map (\x -> x - max) xs
    max = maximum xs

flipweights_ld :: [(a, Log Double)] -> RVar a
flipweights_ld weights = weightedCategorical weights'
    where weights' = [(ln $ exp (p / maxp), idx) | (idx, p) <- weights]
          maxp = maximum $ map snd weights

choose :: Int -> Int -> Log Double
choose n k = Exp $ logFactorial n - logFactorial k - logFactorial (n-k)

log_domain :: (Floating a) => a -> Log a
log_domain = Exp . log

direct_domain :: (Floating a) => Log a -> a
direct_domain = exp . ln

beta :: Double -> Double -> Log Double
beta a b = Exp $ logBeta a b

gamma :: Double -> Log Double
gamma x = Exp $ logGamma x

-- gamma_inc a x
--
--      Log-domain ratio of gamma plus increment to gamma.  This is
--      equivalent to
--
--              gamma (a + x) / gamma a
--
--      but (will in the future be) computed more stably when x <<< a,
--      by a Lanczos series expansion of log gamma.
--
gamma_inc :: Double -> Double -> Log Double
gamma_inc a x = gamma (a + x) / gamma a

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

nullify :: (a -> Bool) -> a -> Maybe a
nullify null thing | null thing = Nothing
                   | otherwise = Just thing

-- `non x` lifts an a -> a function to the Maybe a representation of
-- the space "a except x".  The name is taken from Control.Lens.non
non :: (Eq a) => a -> (a -> a) -> Maybe a -> Maybe a
non x f = nullify (== x) . f . fromMaybe x
