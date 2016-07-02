
-- | Evaluation state for Datum Scripts.
module Datum.Script.Eval.State where
import Datum.Script.Eval.Env            (Env, Thunk(..), PAP(..))
import Datum.Script.Core.Exp
import qualified Datum.Script.Eval.Env  as Env


-------------------------------------------------------------------------------
-- | Evaluation state of an expression,
--   for a CEK-like machine.
data State
        = State
        { -- | Environment.
          stateEnv      :: !Env

          -- | Current context (Kontinuation).
        , stateContext  :: !Context

          -- | Current focus of evaluation (Control).
        , stateControl  :: !Control
        }

deriving instance Show State


-- | Control of state machine.
data Control
        = ControlExp !Exp
        | ControlPAP !PAP

deriving instance Show Control


-- | Context of evaluation.
data Context
        = ContextNil

        -- | In an application we are evaluating the functional expression,
        --   and the frame holds the unevaluated argument.
        | ContextAppArg  !Thunk !Context

        -- | In an application we are evaluating the argument,
        --   and the frame holds the evaluated function.
        | ContextAppFun  !Thunk !Context

deriving instance Show Context


-- | Yield an initial evaluation state for the given expression.
stateInit :: Exp -> State
stateInit xx
        = State 
        { stateEnv      = Env.empty
        , stateContext  = ContextNil 
        , stateControl  = ControlExp xx }


-------------------------------------------------------------------------------
pattern VVPrim p        = VPAP (PAP p [])

pattern VName n         = VVPrim (PVName     n)
pattern VList t xs      = VVPrim (PVList     t xs)
pattern VForest f       = VVPrim (PVForest   f)
pattern VTree t         = VVPrim (PVTree     t)
pattern VTreePath ts    = VVPrim (PVTreePath ts)
pattern VFilePath fp    = VVPrim (PVFilePath fp)

pattern VUnit           = VVPrim (PVAtom  AUnit)
pattern VBool    x      = VVPrim (PVAtom (ABool    x))
pattern VInt     x      = VVPrim (PVAtom (AInt     x))
pattern VFloat   x      = VVPrim (PVAtom (AFloat   x))
pattern VNat     x      = VVPrim (PVAtom (ANat     x))
pattern VDecimal x      = VVPrim (PVAtom (ADecimal x))
pattern VText    x      = VVPrim (PVAtom (AText    x))
pattern VTime    x      = VVPrim (PVAtom (ATime    x))

