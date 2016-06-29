
module Main where

import Config
import Load
import Pervasive
-- import Datum.Script.Core.Exp.Compounds
-- import Datum.Script.Eval
import Data.Default
import Text.Show.Pretty
import qualified Datum.Script.Source.Exp                as Source
import qualified System.Environment                     as System


main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def

        let Just filePath = configFile config

        -- Load the pervasives.
        --  This defines standard utility functions like 'apply'.
        modPervasive    <- loadSource False "Pervasive" strPervasive

        -- Read the client module.
        strSource       <- readFile filePath
        modClient       <- loadSource (configDump config) filePath strSource

        -- Append pervasives to the front of the client module.
        let modTotal    = Source.globModules modPervasive modClient
        putStrLn $ ppShow modTotal

        return ()


{-
eval state
 = do   -- putStrLn $ ppShow state
        result  <- step state
        case result of 
         Left  err              -> error $ show err
         Right state'
          | isDone state'       -> return state'
          | otherwise           -> eval state'

-}