
module Datum.Data.Tree.Operator.Trees
        ( HasTrees (..)
        , mapTrees
        , filterTrees)
where
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Filter
import Datum.Data.Tree.Exp


class HasTrees obj where
 -- | Take the sub-trees of an object.
 takeTrees      :: obj c -> [Tree c]

 -- | Apply a function to every sub-tree of an object,
 --   using the path from the root node.
 mapTrees'      :: (Path -> Tree c -> Tree c')
                ->  Path -> obj  c -> obj 'X

 -- | Filter sub-trees of an object,
 --   using the path from the root node.
 filterTrees'   :: (Path -> Tree c -> Bool)
                ->  Path -> obj  c -> obj c


instance HasTrees Tree where
 takeTrees      = treesOfTree
 mapTrees'      = mapTreesOfTree
 filterTrees'   = filterTreesOfTree


instance HasTrees Forest where
 takeTrees      = treesOfForest
 mapTrees'      = mapTreesOfForest
 filterTrees'   = filterTreesOfForest


-- | Apply a function to every sub-tree of an object.
mapTrees :: HasTrees obj => (Tree c -> Tree c') -> obj c -> obj 'X
mapTrees f o    = mapTrees' (\_ t -> f t) mempty o  
{-# INLINE mapTrees #-}


-- | Filter the sub-trees of an object.
filterTrees :: HasTrees obj => (Tree c -> Bool)   -> obj c -> obj c
filterTrees f o = filterTrees' (\_ t -> f t) mempty o
{-# INLINE filterTrees #-}



