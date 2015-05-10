{-# LANGUAGE RecordWildCards #-}

import Data.Random.RVar
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

type ClusterID = Int
type DatumID = Int

data DPMM = DPMM { components :: Map ClusterID NIG
                 , assignment :: Map DatumID ClusterID
                 , dataset :: Map DatumID Double 
                 }

data NIG = NIG { count :: Int
               , sumx :: Double
               , sumxsq :: Double
               }

bogoinit :: [Double] -> DPMM
bogoinit dataset = DPMM (M.singleton 0 component) assignment dataset'
    where component = NIG (length dataset) (sum dataset) (sum $ map (\ x -> x*x) dataset)
          assignment = M.fromList [(idx, 0) | idx <- [0..length dataset - 1]]
          dataset' = M.fromList (zip [0..] dataset)

gibbsSweep :: DPMM -> RVar DPMM
gibbsSweep = undefined 

uninc :: DatumID -> DPMM -> DPMM 
uninc idx DPMM{..} = DPMM components' (M.delete idx assignment) (M.delete idx dataset)
    where components' = M.update (rmdatum datum) componentIdx components
          datum = fromJust $ M.lookup idx dataset
          componentIdx = fromJust $ M.lookup idx assignment

reinc :: Double -> DatumID -> ClusterID -> DPMM -> DPMM
reinc datum datumIdx componentIdx DPMM{..} = DPMM components' assignment' dataset'
    where assignment' = M.insert datumIdx componentIdx assignment
          dataset' = M.insert datumIdx datum dataset
          components' = M.alter (addDatum datum) componentIdx components

rmdatum :: Double -> NIG -> Maybe NIG
rmdatum x NIG{count=1} = Nothing
rmdatum x NIG{count=ct, sumx=sx, sumxsq=sx2} = Just $ NIG (ct - 1) (sx - x) (sx2 - x*x)

addDatum :: Double -> Maybe NIG -> Maybe NIG
addDatum x Nothing = Just $ NIG 1 x $ x*x
addDatum x (Just NIG{count=ct, sumx=sx, sumxsq=sx2}) = Just $ NIG (ct + 1) (sx + x) (sx2 + x*x)

