
module Job.Exec where
import Job.Create

import Data.Maybe
import BuildBox.Build                   as BB
import BuildBox.Command.System          as BB
import System.IO
import System.Directory
import Control.Monad.IO.Class


---------------------------------------------------------------------------------------------------
-- | Execute a single test job.
execJob :: Job -> Build ()
execJob (JobExec cmdName command mPathInput mPathArgs mPathOutExpected mPathErrExpected)
 = do   
        liftIO  $  putStrLn 
                $  "* "  ++ cmdName
                         ++ replicate (20 - length cmdName) ' '
                ++ fromMaybe "" mPathInput
                ++ fromMaybe "" mPathArgs

        mInput  <- case mPathInput of
                        Nothing         -> return Nothing
                        Just pathInput  
                         -> do  str     <- liftIO $ readFile pathInput
                                return  $ Just str

        mArgs   <- case mPathArgs of
                        Nothing         -> return Nothing
                        Just pathArgs   
                         -> do  str     <- liftIO $ readFile pathArgs
                                return  $ Just $ unwords $ words $ stripHashComments str

        let cmd         = maybe command (\args  -> command ++ " " ++ args)  mArgs
        let input       = maybe ""      (\input -> stripHashComments input) mInput

        (code, outActual, errActual) 
         <- systemTee False cmd input

        doResult cmdName code   
                outActual mPathOutExpected
                errActual mPathErrExpected


---------------------------------------------------------------------------------------------------
-- | Handle the test result.
doResult :: FilePath 
         -> ExitCode 
         -> String -> Maybe String 
         -> String -> Maybe String 
         -> Build ()

doResult name code 
         outActual mOutExpected 
         errActual mErrExpected
 = case code of
        ExitFailure code 
         -> checkFailed  name code outActual mOutExpected errActual mErrExpected

        ExitSuccess
         -> checkCompare name      outActual mOutExpected


-- | Check whether this was an expected error.
checkFailed 
        :: FilePath 
        -> Int 
        -> String -> Maybe String 
        -> String -> Maybe String 
        -> Build ()

checkFailed name code outActual mOutExpected errActual Nothing
 = do   
        liftIO  $ putStrLn 
                $ unlines
                $ [ "! " ++ name
                  , "  Failed with exit code " ++ show code 
                  , outActual 
                  , errActual ]
        liftIO $ exitFailure

checkFailed name code outActual mOutExpected errActual (Just pathErrExpected)
 = do   
        exists  <- liftIO $ doesFileExist pathErrExpected
        if exists
         then do
                expected <- liftIO $ readFile pathErrExpected
                checkDiff name errActual pathErrExpected (Just expected)
         else   checkFailed name code outActual mOutExpected errActual Nothing


-- | Compare expected output of test with actual output.
checkCompare :: FilePath -> String -> Maybe FilePath -> Build ()
checkCompare name outActual (Just pathExpected)
 = do           
        exists  <- liftIO $ doesFileExist pathExpected
        if exists 
         then do
                expected  <- liftIO $ readFile pathExpected
                checkDiff name outActual pathExpected (Just expected)
         else   return ()

checkCompare _ _ Nothing
 =      return ()


-- | Check output of test.
checkDiff :: FilePath -> String -> FilePath -> Maybe String -> Build ()
checkDiff    name got pathExpected (Just expected)
 | got == expected
 = do   return ()

 | otherwise
 = liftIO
 $ do   putStrLn "-- Expected ---------------------------------------"
        putStrLn expected
        putStrLn "-- Got --------------------------------------------"
        putStrLn got
        putStrLn "---------------------------------------------------"
        putStrLn "  (ENTER) continue"
        putStrLn "  (CONTROL-C) quit   (u) update expected"
        handle

 where
        handle 
         = do   line    <- hGetLine stdin
                case line of
                 ""     -> return ()

                 "u"    -> do writeFile pathExpected got 
                              return ()

                 _      -> do putStrLn $ "Unknown action " ++ show line
                              handle


checkDiff   name got _ Nothing
 = return ()


stripHashComments :: String -> String
stripHashComments str
 = unlines $ filter keepLine $ lines str
 where  keepLine ('#' :  _)     = False
        keepLine _              = True


