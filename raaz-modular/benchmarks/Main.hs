import           Data.Version
import           Criterion.Main    (defaultMainWith)
import           Criterion         (bgroup)
import           Criterion.Config  (Config(..), ljust, defaultConfig)
import           Paths_raaz_modular (version)
import           System.Random

import qualified Modules.Number.Modular as Modular

pkgName = "raaz-modular-" ++ showVersion version

main :: IO ()
main = do putStrLn $ "Running benchmarks for " ++ pkgName
          defaultMainWith defaultConfig (return ()) . benchmarks $ (mkStdGen 12345) 

benchmarks gen = [ bgroup "Modular Exponentiation" $ Modular.benchmarks gen ]
