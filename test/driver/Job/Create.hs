
module Job.Create where
import System.FilePath
import Data.Maybe
import Data.List


---------------------------------------------------------------------------------------------------
data Job
        = JobExec
        { jobCommandName        :: FilePath
        , jobCommand            :: String
        , jobInputFile          :: Maybe FilePath
        , jobArgsFile           :: Maybe FilePath
        , jobCompareStdout      :: Maybe FilePath
        , jobCompareStderr      :: Maybe FilePath }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Create test jobs from this list of files.
jobsFromFiles :: [FilePath] -> [Job]
jobsFromFiles fs 
 = concatMap (jobsFromFile fs) fs


-- | Create test jobs for this file.
jobsFromFile   
        :: [FilePath]   -- ^ Files in test directory.
        -> FilePath     -- ^ Current test file to create jobs for.
        -> [Job]

jobsFromFile fs f

 -- Create tests from a .us input file.
 | takeFileName f == "Main.us"
 = [ JobExec
        { jobCommandName        = f
        , jobCommand            = "dist/build/datum-tree/datum-tree " ++ f
        , jobInputFile          = Nothing
        , jobArgsFile           = Nothing
        , jobCompareStdout      = Just $ replaceExtension f ".stdout.check" 
        , jobCompareStderr      = Just $ replaceExtension f ".stderr.check" }
    ]

 | otherwise
 = []

