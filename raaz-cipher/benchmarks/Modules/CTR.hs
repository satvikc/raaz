{-# LANGUAGE DataKinds #-}
module Modules.CTR
       ( benchmarks
       , benchmarksTiny
       ) where

import Criterion.Main

import Raaz.Primitives.Cipher

import Raaz.Cipher.AES.Internal
import Raaz.Cipher.AES.CTR      ()

import Modules.Defaults

benchmarks :: [Benchmark]
benchmarks = benchmarksDefault (ProxyMode :: ProxyMode CTR)

benchmarksTiny :: [Benchmark]
benchmarksTiny = benchmarksTinyDefault (ProxyMode :: ProxyMode CTR)
