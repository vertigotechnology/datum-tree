
import Job.Create
import Job.Exec

import BuildBox.Build                   as BB
import BuildBox.Command.System          as BB
import BuildBox.Command.File            as BB
import System.Environment               as S
import System.Directory                 as S
import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import Data.List


-- | Datum tree test driver.
main :: IO ()
main
 = do   args <- getArgs
        case args of
         [ pathTest ]  
          -> do result  <- BB.runBuild "/tmp" $ runTests pathTest
                case result of
                 Right _        -> return ()
                 Left err       -> print err

         _ -> error "usage: driver <path to tests>"


-- | Run all the tests under the given directory.
runTests :: FilePath -> Build ()
runTests pathTest
 = do   
        isDir   <- liftIO $ S.doesDirectoryExist pathTest
        isFile  <- liftIO $ S.doesFileExist pathTest

        case (isDir, isFile) of
         -- Recurse into directories.
         (True, False)  
          -> do 
                -- Get the list of files 
                fs      <- liftIO 
                        $  fmap (filter (not . isPrefixOf "."))
                        $  S.getDirectoryContents pathTest

                -- Start running the tests in an empty store.
                mapM_ runTests
                 $ map (pathTest </>) fs

         -- Execute a single test.
         (False, True)
          -> do fsSame  <- liftIO 
                        $  fmap (filter (not . isPrefixOf "."))
                        $  S.getDirectoryContents (takeDirectory pathTest)

                let jobs = jobsFromFile fsSame pathTest

                mapM_ execJob jobs

         _ -> error $ "Unexpected file type " ++ pathTest

 