
module Datum.Script.Exp.Compounds
        (expOfPipeline)
where
import Datum.Script.Exp.Core


-- | Combine individule pipeline stages into a combined expression.
expOfPipeline :: [Exp] -> Maybe Exp
expOfPipeline xx
 = make (reverse xx)
 where  make [] = Nothing
        make xs = Just (foldr1 XApp xs)
