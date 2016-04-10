
module Datum.Data.Tree.Operator.Filter
        ( filterTree
        , filterForestsOfTree
        , filterTreesOfForest)
where
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Cast
import Datum.Data.Tree.Operator.Path
import Datum.Data.Tree.Exp
import qualified Data.Repa.Array        as A


-- | Filter a tree by keeping only the sub-trees that match
--   the given predicate. 
--
--   * This is a shallow filter. We only consider the direct sub-trees of
--     the provided one.
--
filterTree :: (Path -> Tree c -> Bool) -> Path -> Tree c -> Tree c
filterTree p path tree 
 = promiseTree
 $ applyForestsOfTree 
        (\ forests 
        -> map  (\forest 
                -> filterTreesOfForest p 
                        (enterForest forest path) 
                        forest)
                forests)
        tree


-- | Filter a tree by keeping only the sub-forests that match
--   the given predicate.
filterForestsOfTree :: (Path -> Forest c -> Bool) -> Path -> Tree c -> Tree c
filterForestsOfTree p path tree
 = promiseTree
 $ applyForestsOfTree 
        (filter (\ forest 
                -> p (enterForest forest path) forest))
        tree


-- | Filter a forest by keeping only the sub-trees that match
--   the given predicate.
filterTreesOfForest :: (Path -> Tree c -> Bool) -> Path -> Forest c -> Forest c
filterTreesOfForest p path forest 
 = promiseForest
 $ applyTreesOfForest
        (filter (\ tree
                -> p (enterTree tree path) tree))
        forest


