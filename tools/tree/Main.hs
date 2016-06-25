
module Main where
import Config
import Datum.Script.Exp.Compounds
import Datum.Script.Eval
import Data.Default
import qualified System.Environment     as System
import Text.Show.Pretty


main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def

        let Just xx  = expOfPipeline (configPipeline config)
        state'  <- eval (stateInit xx)

        putStrLn $ ppShow state'



eval state
 = do   -- putStrLn $ ppShow state
        result  <- step state
        case result of 
         Left  err              -> error $ show err
         Right state'
          | isDone state'       -> return state'
          | otherwise           -> eval state'

