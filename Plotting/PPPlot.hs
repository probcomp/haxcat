{-# LANGUAGE ScopedTypeVariables #-}

module Plotting.PPPlot where

import Data.List (group, sort)
import qualified Data.Map.Strict as M
import Graphics.Rendering.Chart.Easy

-- A "massive" cumulative distribution function is a function that
-- returns a pair: the probability mass up to a, exclusive, and the
-- mass of a itself.
type MassiveCDF a = a -> (Double, Double)

-- Cumulative distribution function: return the mass below a.  The
-- implication is that the probability mass at a is always 0, so there
-- is no difference between treating "below" exclusively or
-- inclusively.  Compare MassiveCDF.
type CDF a = a -> Double

empirical_cdf :: forall a. (Ord a) => [a] -> MassiveCDF a
empirical_cdf xs = lookup where
    tree = fst $ foldl add (M.empty, 0) $ group $ sort xs
    add :: (M.Map a (Double, Double), Double) -> [a] ->
           (M.Map a (Double, Double), Double)
    add (m, tot) same_xs = (m', tot') where
        m' = M.insert (head same_xs) (tot, here) m
        tot' = tot + here
        here = (fromIntegral $ length same_xs) / n
    n = fromIntegral $ length xs
    lookup x = case M.lookupLE x tree of
                 Just (x', (below, there)) -> if x == x' then
                                                  (below, there)
                                              else
                                                  (below + there, 0)
                 Nothing -> (0, 0)

massless :: CDF a -> MassiveCDF a
massless cdf x = (cdf x, 0)

-- Expanded state space for eliminating duplications in discrete
-- distributions.  The idea is to pretend that every value of type a
-- has an infinitesimal 0-1 interval attached to it.
data Expanded a = Expanded a Double
    deriving Eq

instance (Ord a) => Ord (Expanded a) where
    (Expanded x xinf) `compare` (Expanded y yinf) =
        case x `compare` y of
          LT -> LT
          GT -> GT
          EQ -> xinf `compare` yinf

expand_state_space :: MassiveCDF a -> CDF (Expanded a)
expand_state_space mcdf (Expanded x portion) = below + here * portion where
    (below, here) = mcdf x

-- Points suitable for the scatter plot portion of a two-sample P-P plot.
newtype PPScatter = PPScatter [(Double, Double)]
    deriving Show

compute_points :: (Ord a) => [a] -> CDF a -> CDF a -> PPScatter
compute_points xs cdf1 cdf2 = PPScatter $ [(cdf1 x, cdf2 x) | x <- xs]

plot_scatter :: PPScatter -> EC (Layout Double Double) ()
plot_scatter (PPScatter pts) = do
  layout_title .= "Probability-probability plot"
  setColors [opaque red, opaque blue]
  plot $ line "equality line" [[(0,0),(1,1)]]
  plot $ points "observed" pts

deduplicate :: (Ord a) => [a] -> [Expanded a]
deduplicate xs = concatMap annotate $ group $ sort xs where
    annotate same_xs = [Expanded x (i/len) | (x,i) <- zip same_xs [1..]]
        where len = fromIntegral $ length same_xs

data Plottable a = Empirical String [a]
                 | Discrete String (MassiveCDF a)
                 | Continuous String (CDF a)

p_p_plot :: (Ord a) => Plottable a -> Plottable a -> EC (Layout Double Double) ()
p_p_plot pl1 pl2 = do
  plot_scatter $ compute_points (sort $ xs1e ++ xs2e) (cdf_of pl1) (cdf_of pl2)
  layout_x_axis . laxis_title .= describe pl1
  layout_y_axis . laxis_title .= describe pl2
    where xs1e = deduplicate $ sample pl1
          xs2e = deduplicate $ sample pl2
          describe (Empirical name xs) = "Probability of " ++ name ++
              " (" ++ (show $ length xs) ++ " samples)"
          describe (Discrete name _) = "Probability of " ++ name
          describe (Continuous name _) = "Probability of " ++ name
          sample (Empirical _ xs) = xs
          sample (Discrete _ _) = []
          sample (Continuous _ _) = []
          cdf_of (Empirical _ xs) = expand_state_space $ empirical_cdf xs
          cdf_of (Discrete _ cdf) = expand_state_space $ cdf
          cdf_of (Continuous _ cdf) = expand_state_space $ massless cdf

-- import Graphics.Rendering.Chart.Gtk
-- toWindow 300 300 $ p_p_plot [1,2,3] [2,3,4]
-- import Graphics.Rendering.Chart.Backend.Cairo
-- toFile def "example-p-p.png" $ p_p_plot [1,2,3] [2,3,4]
