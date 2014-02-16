{-# LANGUAGE DataKinds #-}
module Modules.ECB
       ( benchmarks
       , benchmarksTiny
       ) where

import Criterion.Main

import Raaz.Primitives.Cipher
import Raaz.Util.Proxy

import Raaz.Cipher.AES.ECB    ()

import Modules.Defaults

benchmarks :: [Benchmark]
benchmarks = benchmarksDefault (Proxy :: Proxy ECB)

benchmarksTiny :: [Benchmark]
benchmarksTiny = benchmarksTinyDefault (Proxy :: Proxy ECB)
