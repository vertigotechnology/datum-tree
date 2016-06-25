
module Datum.Script.Eval.Error
        ( Error         (..)
        , ErrorPrim     (..))
where


-- | Evaluation errors.
data Error
        = Error         String
        | ErrorPrim     ErrorPrim

deriving instance Show Error



-- | Primitive operator errors.
data ErrorPrim
        = ErrorStoreUnknownFileFormat   FilePath
        | ErrorLoadUnknownFileFormat    FilePath

deriving instance Show ErrorPrim