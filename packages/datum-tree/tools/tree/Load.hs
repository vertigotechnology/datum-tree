
module Load where
import Pervasive

import Text.Show.Pretty
import System.FilePath

import qualified Datum.Script.Source.Transform.ToCore   as Source
import qualified Datum.Script.Source.Transform.Defix    as Source
import qualified Datum.Script.Source.Load.Offside       as Source
import qualified Datum.Script.Source.Load.Parser        as Source
import qualified Datum.Script.Source.Load.Lexer         as Source
import qualified Datum.Script.Source.Load.Token         as Source
import qualified Datum.Script.Source.Exp                as Source

import qualified Datum.Script.Core.Exp                  as Core
import qualified Text.Lexer.Inchworm.Char               as I


-------------------------------------------------------------------------------
type Dumper 
        = FilePath -> String -> IO ()

mkDumper :: FilePath -> Dumper
mkDumper filePath name str
 = do
        -- Dump command.
        let dumpName =  takeDirectory filePath 
                    </> takeBaseName filePath
                    <.> "dump-"

        writeFile (dumpName ++ name) str


-------------------------------------------------------------------------------
-- | Load a datum script source file.
loadToSourceModule 
        :: Dumper       -- ^ Dump intermediate representations.
        -> FilePath     -- ^ File path for error messages.
        -> String       -- ^ Text of script source.
        -> IO Source.Module

loadToSourceModule dump filePath strSource
 = do
        -- Tokenise source file.
        result <- Source.scanSource filePath strSource
        let toksSource
                = case result of
                        (toks, _, [])
                         -> toks 
                         ++ [Source.Located
                                filePath (I.Location 0 0) 
                                Source.KEndOfFile]

                        (toks, _, ss)
                         -> error 
                         $  unlines 
                                ["lexical error "
                                , show ss
                                , show toks]

        dump "01-source-scanned.tokens"
         $ ppShow toksSource

        -- Apply the offside rule
        let toksStarts  = Source.addStarts toksSource
        let toksOffside = Source.applyOffside [] [] toksStarts

        dump "02-source-starts.tokens"
         $ ppShow toksStarts

        dump "03-source-offside.tokens"
         $ ppShow toksOffside

        -- Parse the tokens.
        let sourceParsed 
                = case Source.runParser filePath toksOffside Source.pModule of
                        Left  err       -> error $ show err
                        Right src       -> src

        dump "04-source-parsed.us-ast"
         $ ppShow sourceParsed

        dump "05-source-parsed-dennot.us-ast"
         $ ppShow (Source.stripXAnnotM sourceParsed)


        -- Rewrite infix expressions to their prefix form.
        let sourceDefixed
                = case Source.defix
                        Source.defaultFixTable 
                        (Source.startingSourcePos filePath)
                        sourceParsed of
                   Left err     -> error $ show err
                   Right src    -> src

        dump "06-source-defixed.us-ast" 
         $ ppShow sourceDefixed

        dump "07-source-defixed-deannot.us-ast" 
         $ ppShow (Source.stripXAnnotM sourceDefixed)

        return sourceDefixed


-- | Load a datum script as a source expression,
--   appending the pervasives to the front.
loadToSourceExp 
        :: Dumper       -- ^ Dumper for immediate represenations.
        -> FilePath     -- ^ File path for error messages.
        -> String       -- ^ Text of script source.
        -> IO Source.Exp

loadToSourceExp dump filePath strSource
 = do
        -- Load the pervasives.
        --  This defines standard utility functions like 'apply'.
        let dumpNone _ _ = return ()
        modPervasive    <- loadToSourceModule dumpNone "Pervasive" strPervasive

        -- Load the client module.
        modClient       <- loadToSourceModule dump      filePath   strSource

        -- Append pervasives to the front of the client module.
        let modTotal      = Source.globModules modPervasive modClient

        dump "10-source-module.us-ast"
         $ ppShow modTotal

        -- Extract a single expression that represents the client query.
        let Just srcTotal = Source.extractExpOfModule modTotal

        dump "11-source-exp.us-ast"
         $ ppShow srcTotal

        return srcTotal        


-- | Load a datum script source file and convert it to core.
loadToCore
        :: Dumper       -- ^ Dumper for intermediate representations.
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

        dump "20-core-exp.uc-ast"
         $ ppShow xCore

        return xCore

