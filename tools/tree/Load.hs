
module Load where
-- import Datum.Script.Core.Exp.Compounds
-- import Datum.Script.Eval
import Text.Show.Pretty
import Control.Monad
import qualified Datum.Script.Source.Transform.Defix    as Source
import qualified Datum.Script.Source.Parser             as Source
import qualified Datum.Script.Source.Lexer              as Source
import qualified Datum.Script.Source.Token              as Source
import qualified Datum.Script.Source.Exp                as Source


loadSource 
        :: Bool         -- ^ Dump intermediate representations.
        -> FilePath     -- ^ File path for error messages.
        -> String       -- ^ Text of script source.
        -> IO Source.Module

loadSource dump filePath strSource
 = do
        -- Tokenise source file.
        let toksSource   
                =   Source.tokenize filePath strSource
                ++ [Source.Loc filePath 0 0 $ Source.KEndOfFile]
        when dump
         $ writeFile "dump-scanned.tokens"
         $ ppShow toksSource


        -- Parse the tokens.
        let sourceParsed 
                = case Source.runParser filePath toksSource Source.pModule of
                        Left  err       -> error $ show err
                        Right src       -> src

        when dump
         $ writeFile "dump-parsed.us-ast" 
         $ ppShow sourceParsed

        when dump
         $ writeFile "dump-parsed-dennot.us-ast"
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
         $ writeFile "dump-defixed.us-ast" 
         $ ppShow sourceDefixed

        when dump
         $ writeFile "dump-defixed-deannot.us-ast" 
         $ ppShow (Source.stripXAnnotM sourceDefixed)

        return sourceDefixed        
