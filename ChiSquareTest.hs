module ChiSquareTest where

import Control.Monad
import Control.Monad.Trans
import Data.Random
import Data.Random.Distribution.Categorical
import Data.Random.Distribution.ChiSquare
import Data.RVar (sampleRVar, sampleRVarT)
import Graphics.Rendering.Chart.Gtk

import ChiSquare
import Plotting.PPPlot

test_weights :: [(Double, Double)]
test_weights = [(0.25, 1), (0.25, 2), (0.25, 3), (0.25, 4)]

the_p_value :: (Monad m) => Int -> RVarT m Double
the_p_value samples = do
  data1 <- replicateM samples $ categoricalT test_weights
  data2 <- replicateM samples $ categoricalT test_weights
  return $ chi_square_p data1 data2

the_plot :: Int -> Int -> RVarT IO ()
the_plot samples replications = do
  ps <- replicateM replications $ the_p_value samples
  lift $ toWindow 300 300 $ p_p_plot (Continuous "uniform" id)
           (Empirical "p-values" ps)

do_the_plot :: Int -> Int -> IO ()
do_the_plot samples replications = sampleRVarT $ the_plot samples replications

chisq_self_test_plot :: Int -> IO ()
chisq_self_test_plot samples = do
  xs <- sampleRVar $ replicateM samples $ chiSquare 3
  toWindow 300 300 $ p_p_plot (Continuous "cdf" $ cdf $ ChiSquare 3)
               (Empirical "samples" (xs :: [Double]))
