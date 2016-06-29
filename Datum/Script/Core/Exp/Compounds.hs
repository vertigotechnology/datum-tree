
module Datum.Script.Core.Exp.Compounds
        (expOfPipeline)
where
import Datum.Script.Core.Exp.Generic


-- | Combine individule pipeline stages into a combined expression.
expOfPipeline :: [GExp l] -> Maybe (GExp l)
expOfPipeline xx
 = make (reverse xx)
 where  make [] = Nothing
        make xs = Just (foldr1 XApp xs)
