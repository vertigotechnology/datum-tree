
module Datum.Data.Tree.Operator.Slice
        ( sliceTree
        , sliceTreeWithNames)
where
import Datum.Data.Tree.Operator.Path
import Datum.Data.Tree.Operator.Cast
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Exp
import qualified Data.Repa.Array        as A


-- | Slice a tree by keeping only the sub-forests that satisfy the given
--   predicate.
-- 
--   * This is a deep filter, we traverse through the whole tree removing
--     forests that do not match the given predicate.
--
--   * When the predicate returns `False` for a particular forest,
--     both data and meta-data for that forest are removed from the tree.
--
sliceTree :: (Path -> Forest c -> Bool) -> Path -> Tree c -> Tree c
sliceTree p path0 tree0 
 = let path1    = enterTree tree0 path0
   in  promiseTree 
        $ applyForestsOfTree
                (\ forests
                -> map  (\ forest
                        -> let path2    = enterForest forest path1
                           in  applyTreesOfForest 
                                (\ trees
                                -> map  (\ tree 
                                        -> let path3   = enterTree tree path2
                                           in  sliceTree p path3 tree) 
                                        trees)
                                forest)
                $ filter
                        (\ forest 
                        -> let path2    = enterForest forest $ enterTree tree0 path0
                           in  p path2 forest)
                $ forests)
                tree0


-- | Like `sliceTree`, but keep only the forests on the path with the given names.
sliceTreeWithNames :: [Name] -> Tree c -> Tree c
sliceTreeWithNames ns tree
 = sliceTree (\p _ -> onPath ns p) mempty tree


