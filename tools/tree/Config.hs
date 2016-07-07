
module Config where
import Data.Default
import Data.Text                        (Text)
import qualified System.Exit            as System
import qualified Datum.Data.List        as List
import qualified Data.Text              as Text


-------------------------------------------------------------------------------
data Config
        = Config
        { -- Script file.
          configFile            :: Maybe FilePath

          -- Script expression provided on the command line.
        , configExec            :: Maybe Text

          -- Dump intermediate representations.
        , configDump            :: Bool 

          -- Trace interpreter evaluation.
        , configTrace           :: Bool 

          -- Show the unit value in script results, instead of suppressing it.
        , configShowUnit        :: Bool 

          -- Arguments to the script.
        , configArguments       :: [(Text, Text)]
        }


deriving instance Show Config

instance Default Config where
 def    = Config
        { configFile            = Nothing
        , configExec            = Nothing 
        , configDump            = False 
        , configTrace           = False 
        , configShowUnit        = False 
        , configArguments       = [] }


-------------------------------------------------------------------------------
-- | Parse command-line arguments.
parseArgs :: [String] -> Config -> IO Config

parseArgs []   config 
 = return config

parseArgs args config
 | "--dump" : rest      <- args
 = parseArgs rest
 $ config { configDump = True }

 | "--trace" : rest     <- args
 = parseArgs rest
 $ config { configTrace = True }

 | "--show-unit" : rest <- args
 = parseArgs rest
 $ config { configShowUnit = True }

 -- Execute the given expression.
 | "--exec" : strExp : rest <- args
 = parseArgs rest
 $ config { configExec     = Just (Text.pack strExp) }

 -- If we have a script to execute then interpret remaining
 -- flags starting with '-' as arguments to the script.
 | Just _       <- configFile config
 , dparam : value : rest <- args
 , Just param   <- List.stripPrefix "-" dparam
 , Just c       <- List.takeHead param
 , c /= '-'
 = parseArgs rest
 $ config { configArguments 
                =  (configArguments config) 
                ++ [(Text.pack param, Text.pack value)] }

 -- Load script file name.
 | file : rest  <- args
 , Just c       <- List.takeHead file
 , c /= '-'
 = parseArgs rest
 $ config { configFile = Just file }

 -- Some argument that we don't recognise.
 | otherwise
 = do   putStrLn usage
        System.exitFailure


-------------------------------------------------------------------------------
usage
 = unlines
 [ "datum-tree: hierarchical data processing."
 , ""
 , "Usage"
 , " datum-tree <script.us>  [FLAGS..]"
 , " datum-tree --exec <EXP> [FLAGS..]"
 , ""
 , "Execution"
 , " --exec <EXP>        Execute the given script expression."
 , ""
 , "Debugging"
 , " --dump              Dump intermediate representations of script."
 , " --trace             Trace script evaluation."
 , " --show-unit         Show the unit value in script results."
 ]

