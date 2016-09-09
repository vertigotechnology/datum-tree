
module Datum.Script.Core.Eval.Prim.Filter
        (step_Filter)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Data.Tree
import qualified Data.Text      as Text


step_Filter _ _
        PPDropDim 
        [VName name, VForest forest]
 =      
        progress $ VForest
                 $ dropDimOfForest (Text.unpack name) forest

step_Filter _ state _ _
 = crash state