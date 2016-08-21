module Datum.Script.Core.Eval.Prim.World
        (step_World)
where
import Datum.Script.Core.Eval.Prim.Base
import qualified Data.Text              as Text


-- Get the value of a command-line argument.
step_World _ state PPArgument [VText name]
 = case lookup (Text.pack name) $ worldArguments $ stateWorld state of
        Just value     
         -> progress $ VText (Text.unpack value)

        Nothing 
         -> failure $ ErrorPrim $ ErrorArgumentUnknown (Text.pack name)

step_World _ _ _ _
 =      crash