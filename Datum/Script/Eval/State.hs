
-- | Evaluation state for Datum Scripts.
module Datum.Script.Eval.State where
import Datum.Script.Eval.Env            (Env, Value(..))
import Datum.Script.Exp.Core
import qualified Datum.Script.Eval.Env  as Env


-- | Evaluation state of an expression.
data State
        = State
        { -- | Environment mapping the free variables of the expression
          --   to values.
          stateEnv      :: Env

          -- | Context consisting of a stack of evaluation frames.
          --   This records what part of a larger expression we are
          --   currently evaluating.
        , stateContext  :: [Frame]

          -- | Current focus of evaluation, either an expression that 
          --   requires more evaluation, or a value which has finished
          --   evaluation.
        , stateFocus    :: Either Exp Value }

deriving instance Show State


-- | Context of evaluation.
data Frame
        -- | In an application we are evaluating the functional expression,
        --   and the frame holds the unevaluated argument.
        = FrameAppLeft  Exp 

        -- | In an application we are evaluating the argument,
        --   and the frame holds the evaluated function.
        | FrameAppRight Value

deriving instance Show Frame


-- | Check if the evaluation of the expression in the given state is done.
isDone :: State -> Bool
isDone (State _ [] (Right _))   = True
isDone (State _ _  (Left  _))   = False
isDone _                        = False


-- | Yield an initial evaluation state for the given expression.
stateInit :: Exp -> State
stateInit xx
        = State Env.empty [] (Left xx)


pattern VVPrim p        = VPrim p []

pattern VName n         = VVPrim (PVName     n)
pattern VList t xs      = VVPrim (PVList     t xs)
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

