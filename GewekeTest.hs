module GewekeTest where

import Control.Monad.State.Lazy
import Data.Random

import Types
import RowSweep (row_major_to_column_major)
import Predictive
import Haxcat

geweke_transition :: (a -> RVar b) -> (b -> a -> RVar a) -> (a -> RVar a)
geweke_transition mk_data trans init = do
  d <- mk_data init
  trans d init

-- Can I do better than this with an appropriate random stream abstraction?
instrumented_chain :: RVar a -> (a -> RVar a) -> (a -> b) -> Int -> RVar [b]
instrumented_chain _ _ _ 0 = return []
instrumented_chain init step probe k = do
  x <- init
  rest <- instrumented_chain (step x) step probe (k-1)
  return $ (probe x):rest

cc_sample_many :: [RowID] -> Crosscat -> RVar [Row]
cc_sample_many rows cc = evalStateT act cc where
    act = mapM (\r_id -> StateT $ cc_predict r_id) rows

cc_geweke_chain :: [RowID] -> [ColID] -> (Crosscat -> a) -> Int -> RVar [a]
cc_geweke_chain rows cols probe k = instrumented_chain init step probe k where
    init = cc_predict_full cols rows
    step = geweke_transition (cc_sample_many rows) $ \d -> execStateT $ step_act d
    step_act :: [Row] -> StateT Crosscat RVar ()
    step_act d = do
      let row_data = zip rows d
      mapM_ (modifyT . (return .) . uncurry cc_row_only_reinc) row_data
      infer $ row_major_to_column_major d
      mapM_ (modifyT . (return .) . uncurry cc_row_only_uninc) row_data