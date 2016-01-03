module ChiSquareTest where

import Control.Monad
import Control.Monad.Trans
import Data.Random
import Data.Random.Distribution.Categorical
import Data.RVar (sampleRVarT)
import Graphics.Rendering.Chart.Gtk

import ChiSquare
import Plotting.PPPlot

test_weights :: [(Double, Double)]
test_weights = [(0.25, 1), (0.25, 2), (0.25, 3), (0.25, 4)]

the_p_value :: (Monad m) => RVarT m Double
the_p_value = do
  data1 <- replicateM 250 $ categoricalT test_weights
  data2 <- replicateM 250 $ categoricalT test_weights
  return $ chi_square_p data1 data2

the_plot :: RVarT IO ()
the_plot = do
  ps <- replicateM 500 the_p_value
  lift $ toWindow 300 300 $ p_p_plot (Continuous "uniform" id)
           (Empirical "p-values" ps)

do_the_plot :: IO ()
do_the_plot = sampleRVarT the_plot
