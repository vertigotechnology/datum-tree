
module Datum.Script.Core.Eval.Prim.Array
        (step_Array)
where
import Datum.Script.Core.Eval.Prim.Base


step_Array _ _ PPArrayEmpty []
 =      progress $ VVPAF (PVData (PDList XTUnit []))

step_Array _ _ 
         PPArrayExtend 
        [VVPAF p,  VVPAF (PVData (PDList xt xs))]
 =      progress $ VVPAF (PVData (PDList xt (xs ++ [XFrag p])))

step_Array _ _ _ _
 = crash


