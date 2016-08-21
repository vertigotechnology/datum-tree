
module Datum.Script.Core.Eval.Error
        ( Error         (..)
        , ErrorCore     (..)
        , ErrorPrim     (..))
where
import Datum.Script.Core.Exp
import Datum.Script.Core.Eval.State
import Data.Text                                        (Text)
import qualified Datum.Data.Tree.Codec.Matryo.Decode    as Matryo
import qualified Datum.Data.Tree.Check                  as Check


-- | Evaluation errors.
data Error
        = Error         String
        | ErrorCore     ErrorCore
        | ErrorPrim     ErrorPrim

        -- | Evaluator got stuck. The core program is ill-typed.
        | ErrorCrash

deriving instance Show Error


-- | Errors from the ambient core language.
data ErrorCore
        = ErrorCoreStuck
        | ErrorCoreType             State
        | ErrorCoreUnboundVariable  Bound

deriving instance Show ErrorCore


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


deriving instance Show ErrorPrim