{-
This module contain helpers to autotune the performance of various
implementation of the same function.
-}
module Raaz.Util.AutoTune (tune) where

import Raaz.Util.Criterion (compareResults)

import Criterion
import Criterion.Config (defaultConfig)
import Criterion.Monad
import Criterion.Environment (measureEnvironment)
import Data.Function (on)
import Data.List (maximumBy)
import Language.Haskell.TH

-- | It performs the benchmarking for the list benchmarkable items and
-- returns the element corresponding to the benchmark performing the
-- best. Benchmarking is done at compile time and result is returned
-- in the `Q` monad.
tune :: Benchmarkable a
     => [(a,b)] -- ^ benchmarkable item and corresponding element
     -> Q b
tune bs = fmap (snd . maximumBy (compareResults `on` fst)) $ runIO benchmark
    where
      benchmark = sequence $ map (withConfig defaultConfig . singularbench) bs
      singularbench (b,a) = do
        env <- measureEnvironment
        s <- runBenchmark env b
        return (s,a)
