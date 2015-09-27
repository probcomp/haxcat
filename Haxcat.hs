{-# LANGUAGE TupleSections #-}

module Haxcat where

import Control.Monad.State.Lazy
import qualified Data.Map as M

import Data.Random.RVar

import Types
import RowSweep
import ColumnSweep

modifyT :: (Monad m) => (a -> m a) -> StateT a m ()
modifyT f = StateT $ liftM ((),) . f

train :: M.Map ColID (ColumnData Double) -> Int -> RVar Crosscat
train ds k = cc_initialize ds >>= execStateT (replicateM k act) where
    act :: StateT Crosscat RVar ()
    act = do
      modifyT (col_sweep ds)
      modifyT (row_sweep2 ds)
      
