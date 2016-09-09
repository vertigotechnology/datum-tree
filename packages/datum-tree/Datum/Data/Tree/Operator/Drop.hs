
module Datum.Data.Tree.Operator.Drop
        ( dropDimOfTree
        , dropDimOfForest)
where
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Cast
import Datum.Data.Tree.Exp


-- | Drop the named sub-dimension of a tree.
dropDimOfTree :: Name -> Tree 'O -> Tree 'O
dropDimOfTree nDim (Tree (B t0 gs0) (BT n0 tt0 bts0))
 = let
        (gs', bts')
                = unzip
                $ [(g, b) 
                        | g             <- unboxes gs0
                        | b@(BT n _ _)  <- unboxes bts0
                        , n /= nDim]

  in    Tree    (B  t0 (boxes gs'))
                (BT n0 tt0 (boxes bts'))


-- | Drop the named sub-dimension from all trees in a forest.
dropDimOfForest :: Name -> Forest 'O -> Forest 'O
dropDimOfForest nDim forest
 = promiseForest
 $ mapTreesOfForest
        (\_p tree -> dropDimOfTree nDim tree)
        mempty forest 

