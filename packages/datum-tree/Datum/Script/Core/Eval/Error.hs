
module Datum.Script.Core.Eval.Error
        ( Error         (..)
        , ppError

        , ErrorCore     (..)
        , ppErrorCore

        , ErrorPrim     (..)
        , ppErrorPrim)
where
import Datum.Script.Core.Eval.Error.ErrorCore
import Datum.Script.Core.Eval.Error.ErrorPrim
import Text.PrettyPrint.Leijen


-- | Evaluation errors.
data Error
        = Error         String
        | ErrorCore     ErrorCore
        | ErrorPrim     ErrorPrim

deriving instance Show Error



ppError :: Error -> Doc

ppError (Error str)
 = vcat [ text "Runtime error: " <> text str]

ppError (ErrorCore err)
 = vcat [ text "Runtime error in core program."
        , ppErrorCore err ]

ppError (ErrorPrim err)
 = vcat [ text "Runtime error during primitive operator evaluation"
        , ppErrorPrim err ]

