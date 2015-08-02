module Test where

import DPMM
import Data.Random.RVar
import Data.Random.Distribution.Normal
import Control.Monad

yellow :: [Double]
yellow = [-2.8564300066900277,-4.188490584983449,-2.4034375652902247,-1.310045694324143,-2.46509266606692,
 -2.523639652719158,-3.1340541818133594,-1.7146047541678722,-2.638106491456618,-2.482541230368748,
 3.336614758654546,4.001815591824947,2.4128018382717356,3.6998469370221234,4.484083557531247,
 3.0387851838175988,1.7278929048936067,3.685078528164184,2.6630425105163362,2.812163129558648]

type DataGen = Int -> RVar [Double]

-- This is a mixture of two Gaussians (-3/1 and 3/1), implemented by
-- deterministically making half the data points from one and half
-- from the other.
-- Representation: Pass in a count and it generates a list of samples
-- of that length.
gendata :: DataGen
gendata ndata = liftM2 (++) (replicateM (ndata `div` 2) $ normal (-3) 1) (replicateM (ndata `div` 2) $ normal 3 1)


