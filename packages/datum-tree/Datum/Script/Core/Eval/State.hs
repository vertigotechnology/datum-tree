
-- | Evaluation state for Datum Scripts.
module Datum.Script.Core.Eval.State where
import Datum.Script.Core.Eval.Value
import Datum.Script.Core.Eval.Env               (Env)
import Datum.Script.Core.Exp
import Data.Default
import Data.Text                                (Text)
import qualified Datum.Script.Core.Eval.Env  as Env


-------------------------------------------------------------------------------
-- | Evaluation state of an expression,
--   for a CEK-like machine.
data State
        = State
        { -- | World that the machine is executing in.
          stateWorld    :: World

          -- | Environment.
        , stateEnv      :: !Env

          -- | Current context (Kontinuation).
        , stateContext  :: !Context

          -- | Current focus of evaluation (Control).
        , stateControl  :: !Control
        }

deriving instance Show State

stateInit :: World -> Exp -> State
stateInit world xx
        = State
        { stateWorld    = world
        , stateEnv      = Env.empty
        , stateContext  = ContextNil 
        , stateControl  = ControlExp xx }


-------------------------------------------------------------------------------
-- | Information about the state of the outside world that the machine
--   is executing in. This is static configuration information like the 
--   command line arguments that were passed to the script.
data World
        = World
        { -- | Command line arguments passed to the script.
          worldArguments:: [(Text, Text)] 
        }

deriving instance Show World

instance Default World where
 def    = World
        { worldArguments        = [] }


-------------------------------------------------------------------------------
-- | Control of state machine.
data Control
        = ControlExp !Exp
        | ControlPAP !PAP

deriving instance Show Control


-- Convert a `Control` back to an expression,
-- used for error reporting.
expOfControl :: Control -> Exp
expOfControl cc
 = case cc of
        ControlExp xx   -> xx
        ControlPAP pap  -> expOfPAP pap


-------------------------------------------------------------------------------
-- | Context of evaluation.
data Context
        = ContextNil

        -- | In an application we are evaluating the functional expression,
        --   and the frame holds the unevaluated argument.
        | ContextAppArg  !Value !Context

        -- | In an application we are evaluating the argument,
        --   and the frame holds the evaluated function.
        | ContextAppFun  !Value !Context

        -- | We're evalating a list of elements and have already handled
        --   the current ones.
        | ContextList    
        { contextListEnv        :: !Env
        , contextListType       :: !Exp
        , contextListDone       :: ![Value]
        , contextListRest       :: ![Exp]
        , contextListTail       :: !Context }


deriving instance Show Context


-------------------------------------------------------------------------------
pattern VVPAP p         = VPAP (PAP p [])
pattern VVPAF p         = VPAP (PAF p [])

pattern VName n         = VVPAF (PVData (PDName     n))
pattern VRecord fs      = VVPAF (PVData (PDRecord   fs))
pattern VArray t xs     = VVPAF (PVData (PDArray    t xs))
pattern VForest f       = VVPAF (PVData (PDForest   f))
pattern VTree t         = VVPAF (PVData (PDTree     t))
pattern VTreePath ts    = VVPAF (PVData (PDTreePath ts))
pattern VFilePath fp    = VVPAF (PVData (PDFilePath fp))

pattern VUnit           = VVPAF (PVData (PDAtom  AUnit))
pattern VBool    x      = VVPAF (PVData (PDAtom (ABool    x)))
pattern VInt     x      = VVPAF (PVData (PDAtom (AInt     x)))
pattern VFloat   x      = VVPAF (PVData (PDAtom (AFloat   x)))
pattern VNat     x      = VVPAF (PVData (PDAtom (ANat     x)))
pattern VDecimal x      = VVPAF (PVData (PDAtom (ADecimal x)))
pattern VText    x      = VVPAF (PVData (PDAtom (AText    x)))
pattern VTime    x      = VVPAF (PVData (PDAtom (ATime    x)))

