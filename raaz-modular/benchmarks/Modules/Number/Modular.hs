module Modules.Number.Modular (benchmarks) where

import Criterion.Main
import Data.Bits
import System.Random

import Raaz.Number

data Params = Params Integer Integer Integer

benchExponentiation :: Params -> [ Benchmark ]
benchExponentiation p = [ bench "powModuloSlow" $ nf powModuloSlow' p 
                        , bench "powModuloSlowSafe" $ nf powModuloSlowSafe' p
                        ]
  where powModuloSlow' (Params g k m) = powModuloSlow g k m
        powModuloSlowSafe' (Params g k m) = powModuloSlowSafe g k m

benchmarks gen = [ bgroup "Exponentiation" $ benchExponentiation (Params g k m) ]
  where minR = 2
        maxR = 1 `shiftL` 500
        [g, k, m] = (take 3) $ randomRs (minR, maxR) gen
