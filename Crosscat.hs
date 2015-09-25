{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Crosscat where

import qualified Data.Map as M
import qualified Data.Vector as V

import Data.Random.Distribution.Bernoulli (bernoulli)
import qualified Data.Random.Distribution.T as T
import Data.Random.RVar
import Numeric.Log
import Numeric.SpecFunctions (logGamma, logBeta)

import Utils (choose)

newtype RowID = RowID Int
newtype ColID = ColID Int
newtype ViewID = ViewID Int
newtype ClusterID = ClusterID Int

-- Can probably get away with making this unboxed
type ColumnData a = V.Vector a

class Statistic stat element | stat -> element where
    empty :: stat
    insert :: stat -> element -> stat
    remove :: stat -> element -> stat

class (Statistic suffstats element) => ComponentModel hypers suffstats element
        | hypers -> suffstats element where
    update :: hypers -> suffstats -> hypers
    logpdf_marginal :: hypers -> suffstats -> hypers -> Log Double
    logpdf_predictive :: hypers -> element -> Log Double
    sample_predictive :: hypers -> RVar element

    logpdf_predictive hypers x = pdf' / pdf where
        pdf  = logpdf_marginal hypers empty hypers
        pdf' = logpdf_marginal hypers single (update hypers single)
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
    logpdf_marginal (BBM (alpha, beta)) (TFC (t, f)) (BBM (alpha', beta')) =
        choose (t + f) t
          * (Exp $ logBeta alpha' beta') / (Exp $ logBeta alpha beta)
    logpdf_predictive (BBM (alpha, beta)) True =
        Exp $ log $ alpha / (alpha + beta)
    logpdf_predictive (BBM (alpha, beta)) False =
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

    logpdf_marginal hypers GaussStats{..} hypers' =
        (niglognorm hypers' / niglognorm hypers) / (root_2pi ^^ gauss_n)
        where
          -- This is calc_continuous_log_Z from numerics.cpp in crosscat
          -- TODO Copy the actual reference?
          niglognorm :: NIGNormal -> Log Double
          niglognorm NIGNormal{nign_r=r, nign_nu=nu, nign_s=s} = Exp $
              0.5*nu * (log 2 - log s) + 0.5 * log (2*pi) - 0.5 * log r
              + logGamma (nu/2)

    sample_predictive NIGNormal{..} = do
      std_t <- T.t nign_nu
      return $ std_t * scale + nign_mu
        where scale = sqrt(nign_s * (nign_r + 1) / (nign_nu / 2 * nign_r))

data Component = forall hypers suffstats element.
    (ComponentModel hypers suffstats element)
    => Component {
      suffstats :: suffstats,
      hypers :: hypers
    }

-- TODO: I want the model type to determine the data type, but not
-- vice versa (i.e., Gaussians or VonMises can only model Doubles)
data Column datum model = Column (ColumnData datum) (M.Map ClusterID model)

-- I may want to make the dataset a heterogeneous typed data frame along
-- the lines of https://github.com/acowley/Frames but I am not sure I
-- grok the type-level magic.
-- - Apparently there's also Tables http://hackage.haskell.org/package/tables
-- - And "Carter's Library", referenced from
--   https://www.reddit.com/r/haskell/comments/2dd2um/what_are_some_haskell_alternatives_to_pandasnumpy/,
--   which is allegedly online.

type Partition = M.Map RowID ClusterID

-- Choice point: are cluster ids unique or shared across columns?
-- - If unique, then partitions are column-specific, and I have to write
--   code to enforce consistency within a view.
-- - If shared, then a cluster ID does not suffice to determine its
--   suff stats, I also need to know the column.
-- Decision: shared.

data Crosscat = Crosscat { xxx :: M.Map ColID ViewID
                         , yyy :: M.Map ColID (forall d m. Column d m)
                         , zzz :: M.Map ViewID Partition
                         }

-- The hyperparameters we may eventualy be interested in include:
-- - alpha (and d?) for the crp on views
-- - alpha (and d?) for each of the per-view crps
-- - per-column, model-type-dependent hyperparameters (presumably
--   shared among that column's clusters)
--   - currently not obvious from baxcat whether the hypers are per
--     column or per cluster; code for doing inference on them is
--     static and accepts a list of all clusters, but the clusters
--     also store copies (which may have to be kept in sync?).

----------------------------------------------------------------------
-- Column partition transitions                                     --
----------------------------------------------------------------------

-- BaxCat promises to implement three different column transition
-- kernels, named "Gibbs", "MH", and "Aux Gibbs", but doesn't seem to
-- actually read the parameter that is supposed to choose between
-- them.

-- The actual column transition kernel (for collapsed columns)
-- consists of
-- - Trying every extant view,
-- - If not originally a singleton, trying m fresh singleton views
--   each with an independent crp-random partition (giving each a
--   prior prob of alpha/m) (the per-view crp alpha is chosen
--   uniformly at random)
-- - The likelihood function for each view is taken as full_marginal_logp,
--   which is presumably the probability (density) of the data in the column
--   under the clusters given by that view
-- - Picking the new view according to that distribution without a
--   correction.
--   - 9/25/15: I convinced myself that no correction is required,
--     even for moving a column from its singleton, because the
--     probability of proposing back cancels against the prior penalty
--     for having an extra crp partition around.  (Technically this
--     argument requires proposing the crp alpha from the prior as
--     well).
-- - The difference for uncollapsed columns seems to be that the the
--   per-column cluster objects have states in addition to suffstats
--   and (copies of?) the hyper parameters, which they resample
--   e.g. when the column is reassigned.

-- TODO provided the model accepts the data
recompute_suff_stats :: Partition -> ColumnData d -> (M.Map ClusterID model)
recompute_suff_stats = undefined

repartition :: Partition -> Column d m -> Column d m
repartition p (Column d _) =
    -- Methinks hyperparameter values should also just come along for
    -- the ride
    Column d $ recompute_suff_stats p d

column_full_p :: Column d m -> Log Double
column_full_p (Column _ suff_stats) = undefined

-- TODO Will eventually want to do hyperparameter inference on this
view_alpha :: Log Double
view_alpha = Exp 0

col_weights :: Column d m -> Crosscat -> [(ViewID, Log Double)]
col_weights col Crosscat {..} = do
    new_partition <- (undefined :: Partition)
    return $ (new_id, new_p new_partition):existing
        where
          new_id = fst (M.findMax zzz) + 1
          new_p new_partition = (column_full_p $ repartition new_partition col)
                                * view_alpha
          existing = [(v_id, (column_full_p $ repartition partition col)
                               * (Exp $ log (fromIntegral $ x v_id))) |
                      (v_id, partition) <- M.toList zzz]
          x :: ViewID -> Int
          x = undefined -- How many columns does this view already
                        -- contain?  (Not counting the one that's
                        -- being moved now)
