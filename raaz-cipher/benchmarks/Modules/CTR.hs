{-# LANGUAGE DataKinds #-}
module Modules.CTR
       ( benchmarks
       , benchmarksTiny
       ) where

import Criterion.Main

import Raaz.Primitives.Cipher
import Raaz.Util.Proxy

import Raaz.Cipher.AES.CTR    ()

import Modules.Defaults

benchmarks :: [Benchmark]
benchmarks = benchmarksDefault (Proxy :: Proxy CTR)

benchmarksTiny :: [Benchmark]
benchmarksTiny = benchmarksTinyDefault (Proxy :: Proxy CTR)
