
module Datum.Script.Core.Eval.Prim
        ( step, Error(..) )
where
import Datum.Script.Core.Eval.Prim.Array
import Datum.Script.Core.Eval.Prim.Base
import Datum.Script.Core.Eval.Prim.Console
import Datum.Script.Core.Eval.Prim.Fields
import Datum.Script.Core.Eval.Prim.LoadStore
import Datum.Script.Core.Eval.Prim.Map
import Datum.Script.Core.Eval.Prim.Nesting
import Datum.Script.Core.Eval.Prim.Numeric
import Datum.Script.Core.Eval.Prim.Record
import Datum.Script.Core.Eval.Prim.Sample
import Datum.Script.Core.Eval.Prim.Sort
import Datum.Script.Core.Eval.Prim.Traverse
import Datum.Script.Core.Eval.Prim.Union
import Datum.Script.Core.Eval.Prim.World


-- | Evaluate a primitive applied to some arguments.
step    :: (State -> IO (Either Error (Maybe State)))
        -> State
        -> PrimOp -> [Value] -> IO (Either Error Value)

-- Form a thunk from a partially applied primitive.
step _ _ p args
 | length args < arityOfPrimOp p
 = do   progress $ VPAP (PAF (PVOp p) args)

-- Dispatch to the function that handles each primitive.
step self state pp vs
 = case pp of
        -- Numeric        
        PPAdd                   -> step_Numeric   self state pp vs
        PPSub                   -> step_Numeric   self state pp vs
        PPMul                   -> step_Numeric   self state pp vs
        PPDiv                   -> step_Numeric   self state pp vs
        PPNeg                   -> step_Numeric   self state pp vs
        PPEq                    -> step_Numeric   self state pp vs
        PPGt                    -> step_Numeric   self state pp vs
        PPGe                    -> step_Numeric   self state pp vs
        PPLt                    -> step_Numeric   self state pp vs
        PPLe                    -> step_Numeric   self state pp vs

        -- List
        PPArrayEmpty            -> step_Array     self state pp vs
        PPArrayExtend           -> step_Array     self state pp vs

        -- Record
        PPRecordEmpty           -> step_Record    self state pp vs
        PPRecordExtend          -> step_Record    self state pp vs
        PPRecordProject         -> step_Record    self state pp vs

        -- Fields
        PPRenameFields          -> step_Fields    self state pp vs
        PPPermuteFields         -> step_Fields    self state pp vs

        -- Sample
        PPInitial               -> step_Sample    self state pp vs
        PPFinal                 -> step_Sample    self state pp vs
        PPSample                -> step_Sample    self state pp vs

        -- Sort
        PPSortByField           -> step_Sort      self state pp vs

        -- Union
        PPAppend                -> step_Union     self state pp vs
        PPConcat                -> step_Union     self state pp vs

        -- Traverse
        PPAt                    -> step_Traverse  self state pp vs
        PPOn                    -> step_Traverse  self state pp vs

        -- Mapping
        PPMapKeys               -> step_Map       self state pp vs

        -- Nesting
        PPGroup                 -> step_Nesting   self state pp vs
        PPGather                -> step_Nesting   self state pp vs
        PPFlatten               -> step_Nesting   self state pp vs
        PPRenameDimension       -> step_Nesting   self state pp vs

        -- World
        PPArgument              -> step_World     self state pp vs

        -- Console
        PPPrint                 -> step_Console   self state pp vs

        -- LoadStore
        PPLoad                  -> step_LoadStore self state pp vs
        PPStore                 -> step_LoadStore self state pp vs
        PPRead                  -> step_LoadStore self state pp vs

