
module Main where
import Config
import Load
import Data.Default
import Text.Show.Pretty
import Control.Monad
import qualified Datum.Script.Eval              as Eval
import qualified Datum.Script.Eval.Pretty       as Eval
import qualified System.Environment             as System
import qualified System.Exit                    as System
import qualified Data.Text.Lazy.IO              as LText


main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def

        case configFile config of
         Just filePath  -> runScript config filePath
         _              -> do   putStrLn usage
                                System.exitSuccess


-- | Run the script in the given file.
runScript :: Config -> FilePath -> IO ()
runScript config filePath
 = do   -- Read the client module.
        strSource       <- readFile filePath

        -- Parse the client module text and convert to the core language.
        xCore           <- loadToCore (configDump config) filePath strSource

        -- Create a new machine state to evaluate the core expression.
        let state       = Eval.stateInit xCore

        -- Evaluate the script.
        state'         <- eval config state

        let ppConfig    = Eval.Config
                        { Eval.configTreeFormat = Eval.TreeFormatMatryo }
        LText.putStrLn (Eval.pprControl ppConfig $ Eval.stateControl state')
        return ()


-- | Eval loop for a script state.
eval :: Config -> Eval.State -> IO Eval.State
eval config state
 = do   
        when (configTrace config)
         $ putStrLn $ ppShow state

        result  <- Eval.step state
        case result of 
         Left  err              -> error $ show err
         Right Nothing          -> return state
         Right (Just state')    -> eval config state'

