{-# LANGUAGE DataKinds #-}
module Modules.CBC
       ( benchmarks
       , benchmarksTiny
       ) where

import Criterion.Main
import Raaz.Primitives.Cipher
import Raaz.Util.Proxy

import Raaz.Cipher.AES.CBC    ()

import Modules.Defaults

benchmarks :: [Benchmark]
benchmarks = benchmarksDefault (Proxy :: Proxy CBC)

benchmarksTiny :: [Benchmark]
benchmarksTiny = benchmarksTinyDefault (Proxy :: Proxy CBC)
