
module Datum.Script.Core.Eval.Error
        ( Error         (..)
        , ErrorCore     (..)

        , ErrorPrim     (..)
        , ppErrorPrim)
where
import Datum.Script.Core.Exp
import Datum.Script.Core.Eval.State
import Data.Text                                        (Text)
import Text.PrettyPrint.Leijen
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Codec.Matryo.Decode    as Matryo
import qualified Datum.Data.Tree.Check                  as Check
import qualified System.FilePath                        as System


-------------------------------------------------------------------------------
-- | Evaluation errors.
data Error
        = Error         String
        | ErrorCore     ErrorCore
        | ErrorPrim     ErrorPrim


deriving instance Show Error


-------------------------------------------------------------------------------
-- | Errors from the ambient core language.
data ErrorCore
        -- | Evaluator crashed due to some obvious implementation bug
        --   or missing functionality.
        = ErrorCoreCrash

        -- | Evaluation got stuck because the CEK machine did something
        --   unexpected.
        | ErrorCoreStuck

        -- | Evaluation got stuck due to some runtime type error in the 
        --   client program.
        | ErrorCoreType             State

        -- | Evaluation got stuck because we found an unbound variable.
        | ErrorCoreUnboundVariable  Bound


deriving instance Show ErrorCore


-------------------------------------------------------------------------------
-- | Primitive operator errors.
data ErrorPrim
        -- | Command line argument is not specified.
        = ErrorArgumentUnknown
        { errorArgument :: Text  }

        -- | File format is unknown or unhandled.
        | ErrorStoreUnknownFileFormat
        { errorFilePath :: FilePath }

        -- | File format is unknown or unhandled.
        | ErrorLoadUnknownFileFormat
        { errorFilePath :: FilePath }

        -- | Could not decode a tree from the file system.
        | ErrorLoadParseError
        { errorFilePath :: FilePath
        , errorMatryo   :: Matryo.Error }

        -- | Type error when checking a tree from the file system.
        | ErrorLoadTypeError
        { errorFilePath :: FilePath
        , errorCheck    :: Check.Error }

        -- | Type error when appending trees.
        | ErrorAppendTrees
        { errorTrees    :: [T.Tree   'T.O] }

        -- | Type error when appending forests.
        | ErrorAppendForests
        { errorForests  :: [T.Forest 'T.O] }



deriving instance Show ErrorPrim


-- | Pretty print an `ErrorPrim`.
ppErrorPrim :: ErrorPrim -> Doc

ppErrorPrim (ErrorArgumentUnknown arg)
 = vcat [ text "Unknown command line argument " 
                <> (text $ show arg) 
                <> text "." ]

ppErrorPrim (ErrorStoreUnknownFileFormat path)
 = vcat [ text "Unknown file format "
                <> (text $ show $ System.takeExtension path)
                <> text "."
        , text "  when storing: "
                <> (text $ show path) ]

ppErrorPrim (ErrorLoadUnknownFileFormat path)
 = vcat [ text "Unknown file format "
                <> (text $ show $ System.takeExtension path)
                <> text "."
        , text "  when loading: "
                <> (text $ show path) ]

ppErrorPrim (ErrorLoadParseError path err)
 = vcat [ text "Parse error in tree file"
        , text "  when loading: "
                <> (text $ show path)
        , empty
        , text (show err) ]

ppErrorPrim (ErrorLoadTypeError path err)
 = vcat [ text "Type error in tree file"
        , text "  when loading: "
                <> (text $ show path)
        , empty
        , Check.ppError err ]

ppErrorPrim (ErrorAppendTrees _)
 = vcat [ text "Runtime type error when appending trees." ]

ppErrorPrim (ErrorAppendForests _)
 = vcat [ text "Runtime type error when appending forests." ]
