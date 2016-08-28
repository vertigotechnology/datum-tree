
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


step_Union _ _ PPConcat [VList _ vs]
 | Just trees   <- sequence
                $  map (\v -> case v of
                                XTree t -> Just t
                                _       -> Nothing)  vs
 = do   case concatTrees trees of
         Nothing -> crash
         Just t' -> progress $ VTree t'


step_Union _ _ PPConcat [VList _ vs]
 | Just forests <- sequence
                $  map (\v -> case v of
                                XForest f -> Just f
                                _         -> Nothing)  vs
 = case concatForests forests of
        Nothing -> crash
        Just f' -> progress $ VForest f'

step_Union _ _ _ _
 = crash