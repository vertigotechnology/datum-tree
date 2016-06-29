
module Main where
import Config
import Load
import Data.Default
import Text.Show.Pretty
import qualified System.Environment                     as System


main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def

        let Just filePath = configFile config

        -- Read the client module.
        strSource       <- readFile filePath
        xCore           <- loadToCore (configDump config) filePath strSource

        putStrLn $ ppShow xCore

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