
module Datum.Script.Core.Eval.Error.ErrorCore
        ( ErrorCore (..)
        , ppErrorCore)
where
import Datum.Script.Core.Exp
import Datum.Script.Core.Eval.State
import Datum.Script.Core.Eval.Pretty
-- import Datum.Script.Kernel.Exp.Pretty
import Text.PrettyPrint.Leijen
-- import Data.Text                                (Text)
-- import qualified Datum.Script.Kernel.Exp.Bind   as K
-- import qualified Data.Text                      as Text


-------------------------------------------------------------------------------
-- | Errors from the ambient core language.
data ErrorCore
        -- | The expression being evaluated is not in normal form,
        --   but the evaluator cannot make progress.
        = ErrorCoreCrash  State

        -- | Evaluation got stuck because we found an unbound variable.
        | ErrorCoreUnboundVariable  Bound


deriving instance Show ErrorCore


-------------------------------------------------------------------------------
-- | Pretty print an `ErrorCore`.
ppErrorCore :: ErrorCore -> Doc

ppErrorCore (ErrorCoreCrash state)
 = vcat [ text "Interpreter crashed."
        , text "  control: " 
                <>  ppExpCut (expOfControl $ stateControl state) ]

ppErrorCore (ErrorCoreUnboundVariable b)
 = vcat [ text "Unbound variable" 
                <+> text "\"" 
                <>  ppBound b 
                <>  text "\""
                <>  text "."
        ] 


