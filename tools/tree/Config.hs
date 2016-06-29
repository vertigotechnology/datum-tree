
module Config where
import Datum.Script.Core.Exp
import Data.Default
import qualified Data.List      as List
import qualified Data.Text      as Text
import qualified Data.Char      as Char

-------------------------------------------------------------------------------
data Config
        = Config
        { configPipeline        :: [Exp] }

deriving instance Show Config

instance Default Config where
 def    = Config
        { configPipeline        = [] }


pushPipeline :: [String] -> Config -> Exp -> IO Config
pushPipeline rest config xx
 = parseArgs rest 
 $ config { configPipeline = configPipeline config ++ [xx] }


-------------------------------------------------------------------------------
-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs []   config 
 = return config

parseArgs args config
 | "-load"   : file : rest <- args
 = pushPipeline rest config
        $ XApp XLoad  (XFilePath file)

 | "-store"  : file : rest <- args
 = pushPipeline rest config 
        $ XApp XStore (XFilePath file)

 | "-initial" : snat : rest <- args
 , all Char.isDigit snat
 = pushPipeline rest config 
        $ XApp XInitial (XNat (read snat))

 | "-final" : snat : rest <- args
 , all Char.isDigit snat
 = pushPipeline rest config 
        $ XApp XFinal  (XNat (read snat))

 | "-sample" : snat : rest <- args
 , all Char.isDigit snat
 = pushPipeline rest config 
        $ XApp XSample (XNat (read snat))

 | "-gather" : rest        <- args
 , (keys,  rest') <- List.break (List.isPrefixOf "-") rest
 , keys'          <- map Text.pack keys
 = pushPipeline rest' config 
        $ XApp XGather (XTreePath keys')

 | "-group"  : name : rest <- args
 = pushPipeline rest config
        $ XApp XGroup (XName (Text.pack name))

{-
 | "-rename-fields" : rest <- args
 , (names, rest') <- List.break (List.isPrefixOf "-") rest
 , names'         <- map Text.pack names
 = pushPipeline rest' config 
        $ XApp XRenameFields (XList XTName $ map XName names')
-}
 | otherwise
 = error "usage"
