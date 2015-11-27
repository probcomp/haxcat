-- -*- coding: utf-8 -*-

--   Copyright (c) 2010-2014, MIT Probabilistic Computing Project
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

{-# LANGUAGE RecordWildCards #-}

module Plotting.Stub where

import System.Exit (ExitCode)
import System.Process

-- In this hack of a module, a Plot is
-- - a data saver :: FilePath -> IO () that saves the data to plot in a csv
-- - a string of Python souce that, if executed, defines a function
--   named "plot" which accepts a filename and returns a matplotlib
--   figure object representing the plot.

data Plot = Plot { save_data :: FilePath -> IO ()
                 , plot_func_def :: String
                 }

-- Here's a stab at an interface: accept the two column lists and a
-- function that produces the datum, and plot it.
-- Example: Plotting.Stub.show $ cluster_map "foo" "bar" [1,2] [3,4] (+)
cluster_map :: (Show a, Show b) => String -> String
            -> [a] -> [b] -> (a -> b -> Double) -> Plot
cluster_map dim1_name dim2_name dim1_vals dim2_vals f = Plot save plot where
    save file = writeFile file csv_str
    csv_str = unlines $ map tuple_to_str tuples
    tuple_to_str (a, b, c) = a ++ "," ++ b ++ "," ++ c
    tuples = (dim1_name, dim2_name, "value"):concatMap minor dim1_vals
    minor major_val = map (cell major_val) dim2_vals
    cell major_val minor_val = ( Prelude.show major_val, Prelude.show minor_val
                               , Prelude.show $ f major_val minor_val)
    plot = clustermap_plot_func dim1_name dim2_name

-- Display a plot interactively (in-process)
show :: Plot -> IO ExitCode
show Plot{..} = do
  save_data "/tmp/plot_data.csv"
  system $ "python -c '" ++ plot_func_def
             ++ "plot(\"/tmp/plot_data.csv\")\n"
             ++ "import matplotlib.pylab as plt\n"
             ++ "plt.show()'"

-- Save a plot to the given file name
save :: FilePath -> Plot -> IO ExitCode
save filename Plot{..} = do
  save_data "/tmp/plot_data.csv"
  system $ "python -c '" ++ plot_func_def
             ++ "plot(\"/tmp/plot_data.csv\").savefig(\"" ++ filename ++ "\")'"

test_plot :: Plot
test_plot = Plot save_data (clustermap_plot_func "column_a" "column_b") where
    save_data file = do
      writeFile file $ unlines [ "column_a,column_b,value"
                               , "1,1,1"
                               , "1,2,2"
                               , "2,1,3"
                               , "2,2,4"
                               ]

clustermap_plot_func :: String -> String -> String
clustermap_plot_func dim1 dim2 = unlines plot_func_def where
    plot_func_def =
        [ "def plot(name):"
        , "  import pandas as pd"
        , "  import seaborn as sns"
        , "  data = pd.read_csv(name)"
        , "  data = data.pivot(\"" ++ dim1 ++ "\", \"" ++ dim2 ++ "\", \"value\")"
        , "  return sns.clustermap(data)"
        ]
