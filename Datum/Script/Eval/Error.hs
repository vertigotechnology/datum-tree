
module Datum.Script.Eval.Error
        ( Error         (..)
        , ErrorCore     (..)
        , ErrorPrim     (..))
where
import Datum.Script.Core.Exp


-- | Evaluation errors.
data Error
        = Error         String
        | ErrorCore     ErrorCore
        | ErrorPrim     ErrorPrim

deriving instance Show Error

-- | Errors from the ambient core language.
data ErrorCore
        = ErrorCoreUnboundVariable      Bound

deriving instance Show ErrorCore

-- | Primitive operator errors.
data ErrorPrim
        = ErrorStoreUnknownFileFormat   FilePath
        | ErrorLoadUnknownFileFormat    FilePath

deriving instance Show ErrorPrim