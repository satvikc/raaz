import           Data.Version
import           Criterion.Main    (defaultMainWith)
import           Criterion         (bgroup)
import           Criterion.Config  (Config(..), ljust, defaultConfig)
import           Paths_raaz_hash   (version)

import qualified Modules.Sha       as Sha

import           Modules.Defaults

pkgName = "raaz-cipher-" ++ showVersion version

myConfig :: Config
myConfig = defaultConfig {
  cfgSamples = ljust 10
  }

main :: IO ()
main = do putStrLn $ "Running benchmarks for " ++ pkgName
          putStrLn $ "Data Size : " ++ show nSize
          defaultMainWith myConfig (return ()) benchmarks

benchmarks= [ bgroup "SHA" Sha.benchmarks ]
