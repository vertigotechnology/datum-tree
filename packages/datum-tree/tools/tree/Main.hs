
module Main where
import Load
import Pipeline
import Data.Default
import Text.Show.Pretty
import Control.Monad
import Datum.Script.Core.Exp                    (Exp)
import Config                                   (Config(..))
import Data.Maybe
import qualified Config                         as Config

import qualified Datum.Script.Core.Exp          as Exp
import qualified Datum.Script.Core.Eval         as Eval
import qualified Datum.Script.Core.Eval.Pretty  as Eval
import qualified Datum.Script.Core.Eval.Env     as Eval
import qualified Datum.Script.Core.Eval.Error   as Eval

import qualified System.Environment             as System
import qualified System.Exit                    as System
import qualified System.IO                      as System

import qualified Data.Text.Lazy.IO              as LText
import qualified Data.Text                      as Text


-------------------------------------------------------------------------------
main
 = do   args    <- System.getArgs 
        config  <- Config.parseArgs args def
        dispatch config


dispatch config
 -- Can't supply both a script file path and an expression
 -- on the command line, as we don't know which one to use.
 | sum  [ if isJust $ configFile config         then 1 else 0
        , if isJust $ configExec config         then 1 else 0
        , if not $ null $ configPipeline config then 1 else 0 ]
   > (1 :: Int)
 = do   putStrLn Config.usage
        System.exitFailure
 
 -- Run a script from a file.
 | Just filePath        <- configFile config
 =      runScriptFromFile config filePath

 -- Execute a pipeline defined on the command line.
 | stages               <- configPipeline config
 , not $ null stages
 = do   
        -- Dummy file path to use in error messages.
        let filePath    = "<command-line>"

        -- Build a core expression from the stage definitions.
        let Just xCore  = Exp.expOfPipeline
                        $ map expOfStage stages

        runExp config filePath xCore

 -- Execute an expression provided on the command line.
 | Just strSource       <- configExec config
 = do   
        -- Dummy file path to use in error messages.
        let filePath    = "<command-line>"

        -- The source we have is for an expression, so assign it 
        -- to the main variable so it it will parse as a complete script.
        runScript config filePath
                $  "main = "
                ++ Text.unpack strSource
                ++ ";"

 -- We don't have a script to execute.
 | otherwise
 = do   putStrLn Config.usage
        System.exitSuccess


-------------------------------------------------------------------------------
-- | Run the script in the given file.
runScriptFromFile :: Config -> FilePath -> IO ()
runScriptFromFile config filePath
 = do   -- Read the client module.
        strSource       <- readFile filePath
        runScript config filePath strSource


-- | Run the script in the given file.
runScript :: Config -> FilePath -> String -> IO ()
runScript config filePath strSource
 = do   
        -- Parse the client module text and convert to the core language.

        let dumper      
                | configDump config     = mkDumper filePath
                | otherwise             = \_ _ -> return ()
        xCore   <- loadToCore dumper filePath strSource
        runExp config filePath xCore


-- | Run the given core expression.
runExp   :: Config -> FilePath -> Exp -> IO ()
runExp config _filePath xCore
 = do
        -- Create a new machine state to evaluate the core expression.
        let world       = Eval.World 
                        { Eval.worldArguments   = configArguments config }

        let state       = Eval.stateInit world xCore

        -- Evaluate the state.
        state'         <- eval config state

        case Eval.stateControl state' of
         -- Suppress printing of unit value.
         -- Many scripts store their results to some destination file, 
         -- and we don't need the '()' from the final store operation
         -- printed to the console. The GHCi console also does this.
         Eval.ControlPAP (Eval.PAF  (Exp.PVData (Exp.PDAtom Exp.AUnit)) [])
          |  not $ configShowUnit config
          -> return ()

         Eval.ControlExp (Exp.XFrag (Exp.PVData (Exp.PDAtom Exp.AUnit)))
          |  not $ configShowUnit config
          -> return ()

         -- Evaluation has produced some non-unit result, 
         -- so print it to the console.
         _ -> do let ppConfig  = Eval.Config

                 LText.putStrLn 
                        $ Eval.pprControl ppConfig 
                        $ Eval.stateControl state'

                 return ()


-------------------------------------------------------------------------------
-- | Eval loop for a script state.
eval :: Config -> Eval.State -> IO Eval.State
eval config state
 = do   
        when (configTrace config)
         $ putStrLn $ ppShow state

        result  <- Eval.step state
        case result of 
         Left  err              -> errorEval state err
         Right Nothing          -> return state
         Right (Just state')    -> eval config state'


errorEval :: Eval.State -> Eval.Error -> IO a
errorEval state err
 = case err of
        Eval.Error str       
         -> error str

        Eval.ErrorCore errCore
         -> error $ "Core error " ++ show errCore
                ++ ppShow state

        Eval.ErrorPrim errPrim
         -> do  System.hPutStr System.stderr
                 $ "tree: Runtime error during query evaluation.\n\n"
                 ++ show (Eval.ppErrorPrim errPrim)
                 ++ "\n\n"

                System.exitFailure



