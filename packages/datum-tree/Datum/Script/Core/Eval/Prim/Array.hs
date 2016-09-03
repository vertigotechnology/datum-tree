
module Datum.Script.Core.Eval.Prim.Array
        (step_Array)
where
import Datum.Script.Core.Eval.Prim.Base


step_Array _ _ PPArrayEmpty []
 =      progress $ VVPAF (PVData (PDArray XTUnit []))

step_Array _ _ 
         PPArrayExtend 
        [VVPAF (PVData p),  VVPAF (PVData (PDArray xt xs))]
 =      progress $ VVPAF (PVData (PDArray xt (p : xs)))

step_Array _ state _ _
 = crash state


