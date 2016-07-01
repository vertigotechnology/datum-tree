
module Config where
import Data.Default
import qualified Datum.Data.List        as List


-------------------------------------------------------------------------------
data Config
        = Config
        { -- Script file.
          configFile            :: Maybe FilePath

          -- Dump intermediate representations.
        , configDump            :: Bool 

          -- Trace interpreter evaluation.
        , configTrace           :: Bool }

deriving instance Show Config

instance Default Config where
 def    = Config
        { configFile            = Nothing
        , configDump            = False 
        , configTrace           = False }


-------------------------------------------------------------------------------
-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config

parseArgs []   config 
 = return config

parseArgs args config
 | "-dump" : rest       <- args
 = parseArgs rest
 $ config { configDump = True }

 | "-trace" : rest       <- args
 = parseArgs rest
 $ config { configTrace = True }


 | file : rest          <- args
 , Just c               <- List.takeHead file
 , c /= '-'
 = parseArgs rest
 $ config { configFile = Just file }

 | otherwise
 = error "usage derp"
