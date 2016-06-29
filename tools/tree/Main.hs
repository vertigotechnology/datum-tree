
module Main where
import Config
import Load
import Data.Default
import Text.Show.Pretty
import qualified Datum.Script.Eval      as Eval
import qualified System.Environment     as System

main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def

        let Just filePath = configFile config

        -- Read the client module.
        strSource       <- readFile filePath

        -- Parse the client module text and convert to the core language.
        xCore           <- loadToCore (configDump config) filePath strSource

        -- Create a new machine state to evaluate the core expression.
        let state       = Eval.stateInit xCore

        -- Evaluate the script.
        state'          <- eval state

        putStrLn $ ppShow state'


eval state
 = do   putStrLn $ ppShow state
        result  <- Eval.step state
        case result of 
         Left  err              -> error $ show err
         Right state'
          | Eval.isDone state'  -> return state'
          | otherwise           -> eval state'

