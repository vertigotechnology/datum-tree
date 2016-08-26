
module Datum.Script.Core.Eval.Prim.Union
        (step_Union)
where
import Datum.Data.Tree.Operator.Append
import Datum.Script.Core.Eval.Prim.Base


-- The append function does a runtime type check to ensure that the trees
-- being appended have the same types.
step_Union _ _ PPAppend [VTree t1,   VTree t2]
 = case appendTrees t1 t2 of
        Nothing -> crash
        Just t' -> progress $ VTree t'


step_Union _ _ PPAppend [VForest f1, VForest f2]
 = case appendForests f1 f2 of
        Nothing -> crash
        Just f' -> progress $ VForest f'


step_Union _ _ _ _
 = crash