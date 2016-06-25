
module Config where
import Datum.Script.Exp.Core
import Data.Default
import qualified Data.List      as List
import qualified Data.Text      as Text


---------------------------------------------------------------------------------------------------
data Config
        = Config
        { configPipeline        :: [Exp] }

deriving instance Show Config

instance Default Config where
 def    = Config
        { configPipeline        = [] }


pushPipeline :: Config -> Exp -> Config
pushPipeline config xx
 = config { configPipeline = configPipeline config ++ [xx] }


---------------------------------------------------------------------------------------------------
-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs []   config 
 = return config

parseArgs args config
 | "-load"   : file : rest <- args
 = parseArgs rest  $ pushPipeline config (XApp XLoad  (XFilePath file))

 | "-store"  : file : rest <- args
 = parseArgs rest  $ pushPipeline config (XApp XStore (XFilePath file))

 | "-gather" : rest        <- args
 , (keys, rest') <- List.break (List.isPrefixOf "-") rest
 , keys'         <- map Text.pack keys
 = parseArgs rest' $ pushPipeline config (XApp XGather (XTreePath keys'))

 | otherwise
 = error "usage"
