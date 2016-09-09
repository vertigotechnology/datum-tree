
module Datum.Script.Core.Eval.Prim.Reduce
        (step_Reduce)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Data.Tree
import qualified Data.Text      as Text

step_Reduce _ _
        PPCountAsField 
        [VName nField, VName nSub, VTree tree]
 =      progress $ VTree
                 $ countAsFieldTree 
                        (Text.unpack nField)
                        (Text.unpack nSub) 
                        tree


step_Reduce _ _
        PPCountAsField
        [VName nField, VName nSub, VForest forest]
 =      progress $ VForest
                 $ countAsFieldForest
                        (Text.unpack nField)
                        (Text.unpack nSub) 
                        forest


step_Reduce _ state _ _
 = crash state