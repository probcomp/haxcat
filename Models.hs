{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Models where

import Data.Random.Distribution.Bernoulli (bernoulli)
import qualified Data.Random.Distribution.T as T
import Data.Random.RVar
import Numeric.Log
import Numeric.SpecFunctions (logGamma, logBeta)

import Utils (choose)

class Statistic stat element | stat -> element where
    empty :: stat
    insert :: stat -> element -> stat
    remove :: stat -> element -> stat

class (Statistic suffstats element) => ComponentModel hypers suffstats element
        | hypers -> suffstats element where
    update :: hypers -> suffstats -> hypers
    pdf_marginal :: hypers -> suffstats -> Log Double
    pdf_predictive :: hypers -> element -> Log Double
    sample_predictive :: hypers -> RVar element

    pdf_predictive hypers x = pdf' / pdf where
        pdf  = pdf_marginal hypers empty
        pdf' = pdf_marginal hypers single
        single = insert empty x

newtype TFCount = TFC (Int, Int)
instance Statistic TFCount Bool where
    empty = TFC (0, 0)
    insert (TFC (t, f)) True  = TFC (t + 1, f)
    insert (TFC (t, f)) False = TFC (t, f + 1)
    remove (TFC (t, f)) True  = TFC (t - 1, f)
    remove (TFC (t, f)) False = TFC (t, f - 1)

newtype BetaBernoulli = BBM (Double, Double)
instance ComponentModel BetaBernoulli TFCount Bool where
    update (BBM (alpha, beta)) (TFC (t, f)) =
        (BBM (alpha + fromIntegral t, beta + fromIntegral f))
    pdf_marginal h@(BBM (alpha, beta)) s@(TFC (t, f)) =
        choose (t + f) t
          * (Exp $ logBeta alpha' beta') / (Exp $ logBeta alpha beta)
              where  (BBM (alpha', beta')) = update h s
    pdf_predictive (BBM (alpha, beta)) True =
        Exp $ log $ alpha / (alpha + beta)
    pdf_predictive (BBM (alpha, beta)) False =
        Exp $ log $ beta / (alpha + beta)
    sample_predictive (BBM (alpha, beta)) = bernoulli (alpha/(alpha + beta))

data GaussStats = GaussStats {
      gauss_n :: Int,
      gauss_mean :: Double,
      gauss_nvar :: Double
    }
instance Statistic GaussStats Double where
    empty = GaussStats 0 0 0
    insert GaussStats {..} x =
        GaussStats { gauss_n = n',
                     gauss_mean = mean',
                     gauss_nvar = gauss_nvar + delta*(x-mean')
                   } where
            n' = gauss_n + 1
            mean' = gauss_mean + delta / fromIntegral n'
            delta = x - gauss_mean
    -- TODO Unit test that insert and remove are inverses
    -- TODO Can remove be further simplified?
    remove GaussStats {..} x =
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
instance ComponentModel NIGNormal GaussStats Double where
    -- TODO Derive this in terms of the numerically more stable
    -- quantities that GaussStats actually maintains.
    update NIGNormal{..} stats = NIGNormal r' nu' s' mu'
        where
          r' = nign_r + fromIntegral (gauss_n stats)
          nu' = nign_nu + fromIntegral (gauss_n stats)
          mu' = (nign_r*nign_mu + gauss_sum stats) / r'
          s' = nign_s + gauss_sum_sq stats +
               nign_r*nign_mu*nign_mu - r'*mu'*mu'

    pdf_marginal hypers stats@GaussStats{..} =
        (niglognorm hypers' / niglognorm hypers) / (root_2pi ^^ gauss_n)
        where
          hypers' = update hypers stats
          -- This is calc_continuous_log_Z from numerics.cpp in crosscat
          -- TODO Copy the actual reference?
          niglognorm :: NIGNormal -> Log Double
          niglognorm NIGNormal{nign_r=r, nign_nu=nu, nign_s=s} = Exp $
              0.5*nu * (log 2 - log s) + 0.5 * log (2*pi) - 0.5 * log r
              + logGamma (nu/2)

    sample_predictive NIGNormal{..} = do
      -- TODO Find a t distribution that admits real degrees of freedom.
      std_t <- T.t $ floor nign_nu
      return $ std_t * scale + nign_mu
        where scale = sqrt(nign_s * (nign_r + 1) / (nign_nu / 2 * nign_r))
