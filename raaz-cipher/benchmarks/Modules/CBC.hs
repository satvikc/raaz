{-# LANGUAGE DataKinds #-}
module Modules.CBC
       ( benchmarks
       , benchmarksTiny
       ) where

import Criterion.Main
import Raaz.Primitives.Cipher

import Raaz.Cipher.AES.Internal
import Raaz.Cipher.AES.CBC      ()

import Modules.Defaults

benchmarks :: [Benchmark]
benchmarks = benchmarksDefault (ProxyMode :: ProxyMode CBC)

benchmarksTiny :: [Benchmark]
benchmarksTiny = benchmarksTinyDefault (ProxyMode :: ProxyMode CBC)
