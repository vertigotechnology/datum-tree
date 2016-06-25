
module Main where
import Config
import Datum.Script.Exp.Compounds
import Data.Default
import qualified System.Environment     as System
import Text.Show.Pretty

main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def

        putStrLn $ ppShow (expOfPipeline (configPipeline config))

