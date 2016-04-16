
module Datum.Data.Tree.Operator
        ( -- * Casts
          weakenTree
        , weakenForest

          -- * Projections
        , Strippable  (..)
        , Extractable (..)


          -- ** Names
        , takeName
        , hasName

          -- ** Meta-data
        , takeMeta

          -- ** Data
        , takeData

          -- ** Trees
        , takeTrees

          -- ** Forests
        , takeForests

          -- ** Keys
        , takeKey
        , hasKey

          -- ** Fields
        , takeFieldNames
        , elementOfKey
        , hasElement

          -- * Operators

          -- ** Paths
        , enterTree
        , enterForest
        , pathIncludesName
        , onPath 

          -- ** Renaming
        , renameFields

          -- ** Mapping
        , mapTrees
        , mapTrees'

        , mapForests
        , mapForests'

        , mapForestOfTree

          -- ** Filtering
        , filterTrees
        , filterTrees'

        , filterForests
        , filterForests'

          -- * Reducing
        , reduceTree
        , reduceForest

        , keysOfTree
        , sizeOfTree

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
        , initial
        , final
        , sample)
where
import Datum.Data.Tree.Operator.Cast
import Datum.Data.Tree.Operator.Extract
import Datum.Data.Tree.Operator.Fields
import Datum.Data.Tree.Operator.Forests
import Datum.Data.Tree.Operator.Gather
import Datum.Data.Tree.Operator.Group
import Datum.Data.Tree.Operator.Limit
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Path
import Datum.Data.Tree.Operator.Project
import Datum.Data.Tree.Operator.Reduce
import Datum.Data.Tree.Operator.Slice
import Datum.Data.Tree.Operator.Strip
import Datum.Data.Tree.Operator.Traverse
import Datum.Data.Tree.Operator.Trees
import Datum.Data.Tree.Compounds

