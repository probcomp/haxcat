import Data.Random.RVar
import Data.Map (Map)
import qualified Data.Map as M

type ClusterID = Int
type DatumID = Int

data DPMM = DPMM { components :: Map ClusterID NIG
                 , assignment :: [ClusterID]
                 , dataset :: [Double]
                 }

data NIG = NIG { count :: Int
               , sumx :: Double
               , sumxsq :: Double
               }

bogoinit :: [Double] -> DPMM
bogoinit dataset = DPMM (M.singleton 0 component) assignment dataset
    where component = NIG (length dataset) (sum dataset) (sum $ map (\ x -> x*x) dataset)
          assignment = replicate (length dataset) 0

gibbsSweep :: DPMM -> RVar DPMM
gibbsSweep = undefined 

rmdatum :: NIG -> Double -> NIG
rmdatum NIG{count=ct, sumx=sx, sumxsq=sx2} x = NIG (ct - 1) (sx - x) (sx2 - x*x)

addDatum :: NIG -> Double -> NIG
addDatum NIG{count=ct, sumx=sx, sumxsq=sx2} x = NIG (ct + 1) (sx + x) (sx2 + x*x)

