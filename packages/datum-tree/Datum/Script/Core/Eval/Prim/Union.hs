
module Datum.Script.Core.Eval.Prim.Union
        (step_Union)
where
import Datum.Data.Tree.Operator.Append
import Datum.Script.Core.Eval.Prim.Base


-- The append function does a runtime type check to ensure that the trees
-- being appended have the same types.
step_Union _ state PPAppend [VTree t1,   VTree t2]
 = case appendTrees t1 t2 of
        Nothing -> crash state
        Just t' -> progress $ VTree t'


step_Union _ state PPAppend [VForest f1, VForest f2]
 = case appendForests f1 f2 of
        Nothing -> crash state
        Just f' -> progress $ VForest f'


step_Union _ state PPConcat [VArray _ vs]
 | Just trees   <- sequence
                $  map (\v -> case v of
                                PDTree t -> Just t
                                _        -> Nothing)  vs
 = do   case concatTrees trees of
         Nothing -> crash state
         Just t' -> progress $ VTree t'


step_Union _ state PPConcat [VArray _ vs]
 | Just forests <- sequence
                $  map (\v -> case v of
                                PDForest f -> Just f
                                _          -> Nothing)  vs
 = case concatForests forests of
        Nothing -> crash state
        Just f' -> progress $ VForest f'

step_Union _ state _ _
 = crash state