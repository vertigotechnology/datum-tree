
module Datum.Script.Core.Eval.Prim.Nesting
        (step_Nesting)
where
import Datum.Script.Core.Eval.Prim.Base
import qualified Datum.Data.Tree                as T
import qualified Datum.Data.Tree.Operator.Cast  as T
import qualified Data.Text                      as Text


-- Gather subtrees.
step_Nesting _ _ PPGather  [VArray _ names, VTree tree]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VTree $ T.gatherTree names' tree


-- Group by a given key.
step_Nesting _ _ PPGroup   [VName name, VForest forest]
 =      progress $ VForest
                 $ T.promiseForest
                 $ T.groupForest (Text.unpack name) forest

step_Nesting _ _ PPFlatten      [VTree tree]
 = do   progress $ VTree $ T.flattenTree tree

step_Nesting _ _ _ _
 =      crash
