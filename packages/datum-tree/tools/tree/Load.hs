
module Load where
import Pervasive

import Text.Show.Pretty
import Control.Monad

import qualified Datum.Script.Source.Transform.ToCore   as Source
import qualified Datum.Script.Source.Transform.Defix    as Source
import qualified Datum.Script.Source.Load.Parser        as Source
import qualified Datum.Script.Source.Load.Lexer         as Source
import qualified Datum.Script.Source.Load.Token         as Source
import qualified Datum.Script.Source.Exp                as Source

import qualified Datum.Script.Core.Exp                  as Core



-- | Load a datum script source file.
loadToSourceModule 
        :: Bool         -- ^ Dump intermediate representations.
        -> FilePath     -- ^ File path for error messages.
        -> String       -- ^ Text of script source.
        -> IO Source.Module

loadToSourceModule dump filePath strSource
 = do
        -- Tokenise source file.
        let toksSource   
                =   Source.tokenize filePath strSource
                ++ [Source.Loc filePath 0 0 $ Source.KEndOfFile]
        when dump
         $ writeFile "dump-01-source-scanned.tokens"
         $ ppShow toksSource


        -- Parse the tokens.
        let sourceParsed 
                = case Source.runParser filePath toksSource Source.pModule of
                        Left  err       -> error $ show err
                        Right src       -> src

        when dump
         $ writeFile "dump-02-source-parsed.us-ast" 
         $ ppShow sourceParsed

        when dump
         $ writeFile "dump-03-source-parsed-dennot.us-ast"
         $ ppShow (Source.stripXAnnotM sourceParsed)


        -- Rewrite infix expressions to their prefix form.
        let sourceDefixed
                = case Source.defix
                        Source.defaultFixTable 
                        (Source.startingSourcePos filePath)
                        sourceParsed of
                   Left err     -> error $ show err
                   Right src    -> src

        when dump
         $ writeFile "dump-04-source-defixed.us-ast" 
         $ ppShow sourceDefixed

        when dump
         $ writeFile "dump-05-source-defixed-deannot.us-ast" 
         $ ppShow (Source.stripXAnnotM sourceDefixed)

        return sourceDefixed


-- | Load a datum script as a source expression,
--   appending the pervasives to the front.
loadToSourceExp 
        :: Bool         -- ^ Dump intermediate representations.
        -> FilePath     -- ^ File path for error messages.
        -> String       -- ^ Text of script source.
        -> IO Source.Exp

loadToSourceExp dump filePath strSource
 = do
        -- Load the pervasives.
        --  This defines standard utility functions like 'apply'.
        modPervasive    <- loadToSourceModule False "Pervasive" strPervasive

        -- Load the client module.
        modClient       <- loadToSourceModule dump filePath strSource

        -- Append pervasives to the front of the client module.
        let modTotal      = Source.globModules modPervasive modClient

        when dump
         $ writeFile "dump-06-source-module.us-ast" 
         $ ppShow modTotal

        -- Extract a single expression that represents the client query.
        let Just srcTotal = Source.extractExpOfModule modTotal

        when dump
         $ writeFile "dump-07-source-exp.us-ast"
         $ ppShow srcTotal

        return srcTotal        


-- | Load a datum script source file and convert it to core.
loadToCore
        :: Bool         -- ^ Dump intermediate representations.
        -> FilePath     -- ^ File path for error messages.
        -> String       -- ^ Text of script source.
        -> IO Core.Exp  

loadToCore dump filePath strSource
 = do
        xSource       
         <- loadToSourceExp dump filePath strSource

        xCore
         <- case Source.toCoreX xSource of
                Left err      -> error $ show err
                Right xCore'  -> return xCore'

        when dump
         $ writeFile "dump-08-core-exp.uc-ast"
         $ ppShow xCore

        return xCore



