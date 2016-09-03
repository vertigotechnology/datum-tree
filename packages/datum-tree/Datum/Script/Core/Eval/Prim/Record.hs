
module Datum.Script.Core.Eval.Prim.Record
        (step_Record)
where
import Datum.Script.Core.Eval.Prim.Base
import qualified Data.List      as List


-- Form an empty record.
step_Record _ _ 
        PPRecordEmpty []
 =      progress $ VVPAF (PVData (PDRecord []))


-- Extend a record with a new field.
step_Record _ _ 
        PPRecordExtend
        [ VVPAF (PTType pt)     -- Type of new field.
        , VName n               -- Name of new field.
        , VVPAF pv              -- Value of new field.
        , VRecord fs ]          -- Existing fields in record.

 | Just pd   <- case pv of
                 PVData d       -> Just d
                 PTType t       -> Just $ PDType (XFrag (PTType t))
                 _              -> Nothing
  = do
        let f'      = PFField n (Just (XFrag (PTType pt))) pd
        progress $ VVPAF (PVData (PDRecord (f' : fs)))


-- Project a single field from a record.
step_Record _ _
        PPRecordProject
        [ VName n, VRecord fs ]

 | Just f   <- List.find (\f -> pffieldName f == n) fs
 =      progress $ VVPAF (PVData (pffieldValue f))


step_Record _ _ _ _
 = crash

