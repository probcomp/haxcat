{-# LANGUAGE RecordWildCards #-}

module DPMM (train_dpmm, bogoinit, gibbsSweep, DPMM(..), NIG) where

import Control.Monad.State.Lazy
import Data.Random.RVar
import Data.Random.Distribution.Categorical (weightedCategorical)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Numeric.SpecFunctions (logGamma)

flipweights :: [(ClusterID, Double)] -> RVar ClusterID
flipweights weights = weightedCategorical [(exp (p - maxp), idx) | (idx, p) <- weights]
    where maxp = maximum $ map snd weights

----------------------------------------------------------------------
-- Normal-Inverse-Gamma Normal component model                      --
----------------------------------------------------------------------

data NIG = NIG { count :: Int
               , sumx :: Double
               , sumxsq :: Double
               } deriving Show

emptyNIG :: NIG
emptyNIG = NIG 0 0 0

-- I assume this is the log density of the given data point in the
-- given NIG cluster, hardcoding the hyperparameters to m = 0, r = 1,
-- nu = 1, s = 1.
logp :: Double -> NIG -> Double
logp datum NIG{..} = -0.5*log (2*pi) + niglognorm r'' nu'' s'' - niglognorm r' nu' s'
    where r' = r + count'
          nu' =  nu + count'
          m' = (r*m + sumx)/r'
          s' = s + sumxsq + r*m*m  - r'*m'*m'
          r'' = r + count' + 1
          nu'' =  nu + count' + 1
          m'' = (r*m + sumx + datum)/r''
          s'' = s + sumxsq + datum*datum + r*m*m  - r''*m''*m''
          m = 0
          r = 1
          nu = 1
          s = 1
          count' = fromIntegral count
          niglognorm :: Double -> Double -> Double -> Double
          niglognorm r nu s = 0.5*(nu+1) * log 2 + 0.5 * log pi - 0.5 * log r - 0.5*nu * log s + logGamma (nu/2)

rmdatum :: Double -> NIG -> Maybe NIG
rmdatum _ NIG{count=1} = Nothing
rmdatum x NIG{count=ct, sumx=sx, sumxsq=sx2} = Just $ NIG (ct - 1) (sx - x) (sx2 - x*x)

addDatum :: Double -> Maybe NIG -> Maybe NIG
addDatum x Nothing = Just $ NIG 1 x $ x*x
addDatum x (Just NIG{count=ct, sumx=sx, sumxsq=sx2}) = Just $ NIG (ct + 1) (sx + x) (sx2 + x*x)

----------------------------------------------------------------------
-- Dirichlet Process Mixture (of NIGs)                              --
----------------------------------------------------------------------

type ClusterID = Int
type DatumID = Int

data DPMM = DPMM { components :: Map ClusterID NIG
                 , assignment :: Map DatumID ClusterID
                 , dataset :: Map DatumID Double
                 } deriving Show

-- Facade

train_dpmm :: [Double] -> Int -> RVar DPMM
train_dpmm input iters = execStateT (replicateM iters gibbsSweepT) $ bogoinit input

-- Initialization

-- Put everything into one component
bogoinit :: [Double] -> DPMM
bogoinit dataset = DPMM (M.singleton 0 component) assignment dataset'
    where component = NIG (length dataset) (sum dataset) (sum $ map (\ x -> x*x) dataset)
          assignment = M.fromList [(idx, 0) | idx <- [0..length dataset - 1]]
          dataset' = M.fromList (zip [0..] dataset)

-- Gibbs sweeps (on cluster assignments)

-- Weight of the datum in every component (inc. a new one)
getweights :: Double -> DPMM -> [(ClusterID, Double)]
getweights datum DPMM{..} = new:existing
    where new = (fst (M.findMax components) + 1, logp datum emptyNIG + 0)
          existing = [(componentIdx, logp datum cmpnt + log (fromIntegral (count cmpnt)))
                      | (componentIdx, cmpnt) <- M.toList components]

-- Gibbs sweep, reassigning every datum
gibbsSweep :: DPMM -> RVar DPMM
gibbsSweep dpmm = execStateT gibbsSweepT dpmm

gibbsSweepT :: StateT DPMM RVar ()
gibbsSweepT = do
  datums <- liftM M.keys $ gets dataset
  mapM_ stepT datums

stepT :: DatumID -> StateT DPMM RVar ()
stepT idx = do
    dpmm <- get
    dpmm' <- lift $ step idx dpmm
    put dpmm'

-- reassign one datum (by DatumID)
step :: DatumID -> DPMM -> RVar DPMM
step idx dpmm = do
    let (datum, dpmm') = uninc idx dpmm
    let weights = getweights datum dpmm'
    clusterid <- flipweights weights
    return $ reinc datum idx clusterid dpmm'

-- unincorporate a datum from its component (and return its value)
uninc :: DatumID -> DPMM -> (Double, DPMM)
uninc idx DPMM{..} = (datum, DPMM components' (M.delete idx assignment) (M.delete idx dataset))
    where components' = M.update (rmdatum datum) componentIdx components
          datum = fromJust $ M.lookup idx dataset
          componentIdx = fromJust $ M.lookup idx assignment

-- reincorporate a datum into the given component
reinc :: Double -> DatumID -> ClusterID -> DPMM -> DPMM
reinc datum datumIdx componentIdx DPMM{..} = DPMM components' assignment' dataset'
    where assignment' = M.insert datumIdx componentIdx assignment
          dataset' = M.insert datumIdx datum dataset
          components' = M.alter (addDatum datum) componentIdx components
