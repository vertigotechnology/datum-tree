
module Datum.Data.Tree.Operator
        ( -- * Casts
          weakenTree
        , weakenForest

          -- * Projections
        , HasName (..)
        , HasMeta (..)
        , HasData (..)

        , forestsOfTree
        , treesOfForest

          -- * Meta-data
        , HasFields(..)

          -- * Data
        , HasTrees (..)

          -- * Paths
        , enterTree
        , enterForest
        , pathIncludesName
        , onPath 

          -- * Mapping
        , mapTreesOfTree
        , mapTreesOfForest
        , mapForestsOfTree
        , mapForestOfTree

          -- * Reducing
        , reduceTree
        , reduceForest

        , keysOfTree
        , sizeOfTree

          -- * Filtering
        , filterTree
        , filterTreesOfForest
        , filterForestsOfTree

          -- * Slicing
        , sliceTree
        , sliceTreeWithNames

          -- * Grouping
        , groupForest

          -- * Gathering
        , gatherTree

          -- * Traversal
        , traverseTree

          -- * Limiting
        , Initial (..)
        , Final   (..)
        , Sample  (..))
where
import Datum.Data.Tree.Operator.Cast
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Path
import Datum.Data.Tree.Operator.Meta
import Datum.Data.Tree.Operator.Project
import Datum.Data.Tree.Operator.Filter
import Datum.Data.Tree.Operator.Limit
import Datum.Data.Tree.Operator.Reduce
import Datum.Data.Tree.Operator.Slice
import Datum.Data.Tree.Operator.Group
import Datum.Data.Tree.Operator.Gather
import Datum.Data.Tree.Operator.Traverse
import Datum.Data.Tree.Compounds
import Datum.Data.Tree.Exp


class HasTrees obj where
 mapTrees :: (Path -> obj c -> obj c')
          ->  Path -> obj c -> obj 'X


instance HasTrees Tree where
 mapTrees = mapTreesOfTree

