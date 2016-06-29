
module Main where
import Config
-- import Datum.Script.Core.Exp.Compounds
-- import Datum.Script.Eval
import Data.Default
import Text.Show.Pretty
import Control.Monad
import qualified Datum.Script.Source.Parser     as Source
import qualified Datum.Script.Source.Lexer      as Source
import qualified Datum.Script.Source.Token      as Source
import qualified Datum.Script.Source.Exp        as Source
import qualified System.Environment             as System


main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def
        let dump = configDump config


        let Just filePath = configFile config

        -- Read the file.
        strSource       <- readFile filePath

        -- Tokenise source file.
        let toksSource   =   Source.tokenize filePath strSource 
                         ++ [Source.Loc filePath 0 0 $ Source.KEndOfFile]
        when dump
         $ writeFile "dump-tokens"
         $ ppShow toksSource

        -- Parse the tokens.
        let sourceParsed 
                = case Source.runParser filePath toksSource Source.pScript of
                        Left  err       -> error $ show err
                        Right src       -> src

        when dump
         $ writeFile "dump-parsed.datum-source-ast" 
         $ ppShow sourceParsed

        when dump
         $ writeFile "dump-parsed-dennot.datum-source-ast"
         $ ppShow (Source.stripXAnnot sourceParsed)

--        let Just xx  = expOfPipeline (configPipeline config)
--        state'  <- eval (stateInit xx)

        putStrLn $ ppShow config


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