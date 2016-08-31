
module Datum.Script.Core.Eval.Prim.Record
        (step_Record)
where
import Datum.Script.Core.Eval.Prim.Base


step_Record _ _ PPRecordEmpty []
 =      progress $ VVPAF (PVData (PDRecord []))

step_Record _ _ 
        PPRecordExtend
        [ VVPAF (PTType pt)
        , VVPAF (PVData (PDName n))
        , VVPAF pv
        , VVPAF (PVData (PDRecord fs)) ]

 | Just pd   <- case pv of
                 PVData d       -> Just d
                 PTType t       -> Just $ PDType (XFrag (PTType t))
                 _              -> Nothing
  = do
        let f'      = PFField n (Just (XFrag (PTType pt))) pd
        progress $ VVPAF (PVData (PDRecord (f' : fs)))

step_Record _ _ _ _
 = crash

