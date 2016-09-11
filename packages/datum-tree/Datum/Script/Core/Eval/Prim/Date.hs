
module Datum.Script.Core.Eval.Prim.Date
        (step_Date)
where
import Datum.Script.Core.Eval.Prim.Base
import qualified Data.Repa.Scalar.Date32        as Date32


step_Date _ _ PPDatePack [VNat yy, VNat mm, VNat dd]
 =      progress $ VDate (Date32.pack (yy, mm, dd))

step_Date _ _ PPDateDiff [VDate d1, VDate d2]
 =      progress $ VInt  (fromIntegral $ Date32.diffDays d2 d1)

step_Date _ state _ _
 = error $ show state