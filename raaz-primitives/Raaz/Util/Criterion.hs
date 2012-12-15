{-
This module contains utility functions to measure performance
-}
module Raaz.Util.Criterion (compareResults) where

import Data.Ord (comparing)
import Statistics.Sample

compareResults :: Sample -> Sample -> Ordering
compareResults = comparing mean