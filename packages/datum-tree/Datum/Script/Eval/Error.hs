
module Datum.Script.Eval.Error
        ( Error         (..)
        , ErrorCore     (..)
        , ErrorPrim     (..))
where
import Datum.Script.Core.Exp
import Datum.Script.Eval.State
import Data.Text                (Text)


-- | Evaluation errors.
data Error
        = Error         String
        | ErrorCore     ErrorCore
        | ErrorPrim     ErrorPrim

deriving instance Show Error


-- | Errors from the ambient core language.
data ErrorCore
        = ErrorCoreStuck
        | ErrorCoreType             State
        | ErrorCoreUnboundVariable  Bound

deriving instance Show ErrorCore


-- | Primitive operator errors.
data ErrorPrim
        = ErrorArgumentUnknown          Text
        | ErrorStoreUnknownFileFormat   FilePath
        | ErrorLoadUnknownFileFormat    FilePath

deriving instance Show ErrorPrim