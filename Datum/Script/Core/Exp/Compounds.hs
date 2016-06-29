
module Datum.Script.Core.Exp.Compounds
        (expOfPipeline)
where
import Datum.Script.Core.Exp


-- | Combine individule pipeline stages into a combined expression.
expOfPipeline :: [Exp] -> Maybe Exp
expOfPipeline xx
 = make (reverse xx)
 where  make [] = Nothing
        make xs = Just (foldr1 XApp xs)
