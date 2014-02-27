{-# LANGUAGE DataKinds #-}
module Modules.ECB
       ( benchmarks
       , benchmarksTiny
       ) where

import Criterion.Main

import Raaz.Primitives.Cipher

import Raaz.Cipher.AES.Internal
import Raaz.Cipher.AES.ECB      ()

import Modules.Defaults

benchmarks :: [Benchmark]
benchmarks = benchmarksDefault (ProxyMode :: ProxyMode ECB)

benchmarksTiny :: [Benchmark]
benchmarksTiny = benchmarksTinyDefault (ProxyMode :: ProxyMode ECB)
