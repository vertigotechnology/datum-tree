
module Datum.Script.Core.Eval.Prim.Record
        (step_Record)
where
import Datum.Script.Core.Eval.Prim.Base


step_Record _ _ PPRecordEmpty []
 =      progress $ VVPAF (PVData (PDRecord []))

step_Record _ _ 
        PPRecordExtend
        [ VVPAF (PVData (PDName n))
        , VVPAF (PVData d)
        , VVPAF (PVData (PDRecord fs)) ]
 = do
        let f'      = PFField n Nothing d
        progress $ VVPAF (PVData (PDRecord (f' : fs)))

step_Record _ _ _ _
 = crash

