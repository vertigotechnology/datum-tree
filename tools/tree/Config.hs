
module Config where
import Data.Default
import qualified System.Exit            as System
import qualified Datum.Data.List        as List


-------------------------------------------------------------------------------
data Config
        = Config
        { -- Script file.
          configFile            :: Maybe FilePath

          -- Dump intermediate representations.
        , configDump            :: Bool 

          -- Trace interpreter evaluation.
        , configTrace           :: Bool 

          -- Show the unit value in script results, instead of suppressing it.
        , configShowUnit        :: Bool 
        }


deriving instance Show Config

instance Default Config where
 def    = Config
        { configFile            = Nothing
        , configDump            = False 
        , configTrace           = False 
        , configShowUnit        = False }


-------------------------------------------------------------------------------
-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config

parseArgs []   config 
 = return config

parseArgs args config
 | "--dump" : rest       <- args
 = parseArgs rest
 $ config { configDump = True }

 | "--trace" : rest     <- args
 = parseArgs rest
 $ config { configTrace = True }

 | "--show-unit" : rest <- args
 = parseArgs rest
 $ config { configShowUnit = True }

 | file : rest          <- args
 , Just c               <- List.takeHead file
 , c /= '-'
 = parseArgs rest
 $ config { configFile = Just file }

 | otherwise
 = do   putStrLn usage
        System.exitFailure


-------------------------------------------------------------------------------
usage
 = unlines
 [ "datum-tree: hierarchical data processing."
 , ""
 , "Usage"
 , " datum-tree [FLAGS..] <script.us>"
 , ""
 , "Debugging"
 , " --dump           Dump intermediate representations of script."
 , " --trace          Trace script evaluation."
 , " --show-unit      Show the unit value in script results."
 ]





