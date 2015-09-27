{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Models where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Data.Random.Distribution.Bernoulli (bernoulli)
import Data.Random.Distribution.Categorical (weightedCategorical)
import qualified Data.Random.Distribution.T as T
import Data.Random.RVar
import Numeric.Log hiding (sum)
import Numeric.SpecFunctions (logGamma)

import Utils (choose, log_domain)
import qualified Utils as U

type PDF elt = elt -> Log Double

class Model m elt | m -> elt where
    pdf :: m -> PDF elt
    sample :: m -> RVar elt

class Statistic stat elt | stat -> elt where
    empty :: stat
    insert :: elt -> stat -> stat
    remove :: elt -> stat -> stat

data NoStat a = NoStat
instance Statistic (NoStat a) a where
    empty = NoStat
    insert _ _ = NoStat
    remove _ _ = NoStat

class (Statistic stat elt) => CompoundModel m stat elt
        | m -> stat elt where
    -- integral of p(t | theta) p(theta | alpha) dtheta
    pdf_marginal :: m -> PDF stat

    -- integral of p(x | t, theta) p(theta | alpha) dtheta
    pdf_predictive :: stat -> m -> PDF elt

    -- theta ~ p(| alpha), x ~ p(| t, theta), yield x
    sample_predictive :: stat -> m -> RVar elt

    pdf_predictive stats m x = pdf' / pdf where
        pdf  = pdf_marginal m stats
        pdf' = pdf_marginal m (insert x stats)

class (Model m elt, CompoundModel m stat elt) => ConjugateModel m stat elt
  where
    update :: stat -> m -> m
    -- update empty = id
    -- (update . a) . (update . b) = update . (a `union` b)

    -- pdf_predictive s = pdf . update s
    -- sample_predictive s = sample . update s

conjugate_pdf_predictive :: (ConjugateModel m stat elt)
    => stat -> m -> elt -> Log Double
conjugate_pdf_predictive suffstats model =
    pdf (update suffstats model)

conjugate_sample_predictive :: (ConjugateModel m stat elt)
    => stat -> m -> RVar elt
conjugate_sample_predictive suffstats model =
    sample (update suffstats model)


newtype TFCount = TFC (Int, Int)
instance Statistic TFCount Bool where
    empty = TFC (0, 0)
    insert True  (TFC (t, f)) = TFC (t + 1, f)
    insert False (TFC (t, f)) = TFC (t, f + 1)
    remove True  (TFC (t, f)) = TFC (t - 1, f)
    remove False (TFC (t, f)) = TFC (t, f - 1)

newtype BetaBernoulli = BBM (Double, Double)
instance Model BetaBernoulli Bool where
    pdf (BBM (alpha, beta)) True  = U.bernoulli_weight alpha beta
    pdf (BBM (alpha, beta)) False = U.bernoulli_weight beta alpha
    sample (BBM (alpha, beta)) = bernoulli (alpha/(alpha + beta))
instance CompoundModel BetaBernoulli TFCount Bool where
    pdf_marginal h@(BBM (alpha, beta)) s@(TFC (t, f)) =
        choose (t + f) t * (U.beta alpha' beta') / (U.beta alpha beta)
            where  (BBM (alpha', beta')) = update s h
    pdf_predictive = conjugate_pdf_predictive
    sample_predictive = conjugate_sample_predictive
instance ConjugateModel BetaBernoulli TFCount Bool where
    update (TFC (t, f)) (BBM (alpha, beta)) =
        (BBM (alpha + fromIntegral t, beta + fromIntegral f))

data GaussStats = GaussStats {
      gauss_n :: Int,
      gauss_mean :: Double,
      gauss_nvar :: Double
    }
instance Statistic GaussStats Double where
    empty = GaussStats 0 0 0
    insert x GaussStats {..} =
        GaussStats { gauss_n = n',
                     gauss_mean = mean',
                     gauss_nvar = gauss_nvar + delta*(x-mean')
                   } where
            n' = gauss_n + 1
            mean' = gauss_mean + delta / fromIntegral n'
            delta = x - gauss_mean
    -- TODO Unit test that insert and remove are inverses
    -- TODO Can remove be further simplified?
    remove x GaussStats {..} =
        GaussStats { gauss_n = n',
                     gauss_mean = mean',
                     gauss_nvar = gauss_nvar - delta*(x-gauss_mean)
                   } where
            n' = gauss_n - 1
            mean' = (fromIntegral gauss_n * gauss_mean - x) / fromIntegral n'
            delta = x - mean'

gauss_sum :: GaussStats -> Double
gauss_sum GaussStats{..} = fromIntegral gauss_n*gauss_mean

gauss_sum_sq :: GaussStats -> Double
gauss_sum_sq GaussStats{..} =
    gauss_nvar + fromIntegral gauss_n*gauss_mean*gauss_mean

root_2pi :: Log Double
root_2pi = Exp $ 0.5 * log (2*pi)

data NIGNormal = NIGNormal {
      nign_r :: Double,
      nign_nu :: Double,
      nign_s :: Double,
      nign_mu :: Double
    }

instance Model NIGNormal Double where
    pdf = undefined -- I'm too lazy, and this will get hidden by the
                    -- default implementation of pdf_predictive
                    -- anyway.

    sample NIGNormal{..} = do
      -- TODO Find a t distribution that admits real degrees of freedom.
      std_t <- T.t $ floor nign_nu
      return $ std_t * scale + nign_mu
        where scale = sqrt(nign_s * (nign_r + 1) / (nign_nu / 2 * nign_r))

instance CompoundModel NIGNormal GaussStats Double where
    pdf_marginal hypers stats@GaussStats{..} =
        (niglognorm hypers' / niglognorm hypers) / (root_2pi ^^ gauss_n)
        where
          hypers' = update stats hypers
          -- This is calc_continuous_log_Z from numerics.cpp in crosscat
          -- TODO Copy the actual reference?
          niglognorm :: NIGNormal -> Log Double
          niglognorm NIGNormal{nign_r=r, nign_nu=nu, nign_s=s} = Exp $
              0.5*nu * (log 2 - log s) + 0.5 * log (2*pi) - 0.5 * log r
              + logGamma (nu/2)

    sample_predictive = conjugate_sample_predictive

instance ConjugateModel NIGNormal GaussStats Double where
    -- TODO Derive this in terms of the numerically more stable
    -- quantities that GaussStats actually maintains.
    update stats NIGNormal{..} = NIGNormal r' nu' s' mu'
        where
          r' = nign_r + fromIntegral (gauss_n stats)
          nu' = nign_nu + fromIntegral (gauss_n stats)
          mu' = (nign_r*nign_mu + gauss_sum stats) / r'
          s' = nign_s + gauss_sum_sq stats +
               nign_r*nign_mu*nign_mu - r'*mu'*mu'

data Counts a c = {
        counts_map :: M.Map a c,
        counts_total :: c
    }
instance (Eq a, Ord a, Num c) => Statistic (Counts a c) a where
    empty = Counts M.empty 0
    insert x Counts{..} = Counts {
            counts_map = M.alter (Just . (+ 1) . maybe 0 id) x counts_map,
            counts_total = counts_total + 1
        }
    remove x Counts{..} = Counts {
            counts_map = M.alter (Just . (+ -1) . maybe 0 id) x counts_map,
            counts_total = counts_total - 1
        }

merge :: (Ord a, Num c) => Counts a c -> Counts a c -> Counts a c
merge (Counts m1 t1) (Counts m2 t2) = Counts {
        counts_map = M.unionWith (+) m1 m2,
        counts_total = t1 + t2
    }

-- CRP is different because it's collapsed without being conjugate.
data CRP a = CRP a Double
instance (Ord a, Enum a) => CompoundModel (CRP a) (Counts a Int) a where
    pdf_marginal = undefined -- TODO This is well-defined, but I'm lazy
    pdf_predictive cs crp x = log_domain $ pdf_predictive_direct_crp cs crp x
    sample_predictive cs crp = weightedCategorical $ crp_weights cs crp

crp_weights :: (Ord a, Enum a) => (Counts a Int) -> (CRP a) -> [(Double, a)]
crp_weights cs crp =
    [(pdf_predictive_direct_crp cs crp x, x) | x <- enumerate_crp cs crp]

pdf_predictive_direct_crp :: (Ord a) => Counts a Int -> CRP a -> a -> Double
pdf_predictive_direct_crp cs@(Counts m _) (CRP _ alpha) x = mine / total where
    mine = fromMaybe alpha $ fmap fromIntegral $ M.lookup x m
    total = alpha + (fromIntegral $ counts_total cs)

enumerate_crp :: (Enum a) => (Counts a Int) -> CRP a -> [a]
enumerate_crp (Counts cs _) (CRP zero _) =
    if M.null cs then
        [zero]
    else
        new : M.keys cs where
            new = succ $ fst $ M.findMax cs

crp_sample_partition :: (Ord a, Ord b, Enum b)
    => (Counts b Int) -> CRP b -> [a] -> RVar (M.Map a b, Counts b Int)
crp_sample_partition cs crp items = foldM sample1 (M.empty, cs) items where
    -- sample1 :: (M.Map a b, Counts b Int)
    --     -> a -> RVar (M.Map a b, Counts b Int)
    sample1 (m, cs) item = do
      new <- sample_predictive cs crp
      return (M.insert item new m, insert new cs)
