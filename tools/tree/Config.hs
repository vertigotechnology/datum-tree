
module Config where
import Pipeline
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

          -- Pipeline of operators defined on the command line.
        , configPipeline        :: [Stage]

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
        , configPipeline        = []
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

 -- Parse pipeline stage
 | stage : rest <- args
 , Nothing      <- configFile config
 , Nothing      <- configExec config
 , Just c       <- List.takeHead stage
 , c == '-'
 = do   (ss, rest') <- parseStage (stage : rest)
        parseArgs rest'
         $ config { configPipeline = configPipeline config ++ [ss] }

 -- Some argument that we don't recognise.
 | otherwise
 = do   putStrLn usage
        System.exitFailure


-------------------------------------------------------------------------------
parseStage :: [String] -> IO (Stage, [String])
parseStage []
 = do   putStrLn usage
        System.exitFailure

parseStage args
 | "-load"   : filePath : rest  <- args
 = return (SLoad  filePath, rest)

 | "-store"  : filePath : rest  <- args
 = return (SStore filePath, rest)

 | "-group"  : rest             <- args
 , ([name], rest') <- splitNames rest
 = do   return (SGroup (Text.pack name), rest')

 | "-gather" : rest             <- args
 , (names, rest')  <- splitNames rest
 = do   let names' = "root" : names
        return (SGather (map Text.pack names'), rest')

 | "-rename-fields" : rest      <- args
 , (names, rest')  <- splitNames rest
 = do   return (SRenameFields (map Text.pack names), rest')

 | "-on" : rest    <- args
 , (names, rest')  <- splitNames rest
 = do   let names' = "root" : names
        (stage', rest'')  <- parseStage rest' 
        return (SOn (map Text.pack names') stage', rest'')

 | otherwise
 = do   putStrLn $ unlines
                [ "Cannot parse stage definition: '" ++ show args ++ "'" ]
        System.exitFailure

splitNames :: [String] -> ([String], [String])
splitNames ss
 = let  names   = takeWhile (not . List.isPrefixOf "-") ss
        rest    = drop (length names) ss
   in   (names, rest)



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

