
-- | Datum data trees.
--
--   * To construct literal trees use either 
--     the     "Datum.Data.Tree.Exp" 
--     or the  "Datum.Data.Tree.SExp" modules.
--
--   * To access the internal representation use
--     the "Datum.Data.Tree.Compounds" module.
--
module Datum.Data.Tree
        ( -- * Objects
          Tree
        , Forest
        , Key
        , Element

          -- * Type Checking
        , Checked (..)
        , Check   (..)
        , Error   (..)

          -- * Operators
          -- ** Projections
        , HasName (..)
        , HasMeta (..)
        , HasData (..)

        , forestsOfTree
        , treesOfForest

          -- ** Paths
        , enterTree
        , enterForest
        , pathIncludesName
        , onPath 

          -- ** Mapping
        , mapTreesOfTree
        , mapTreesOfForest
        , mapForestsOfTree
        , mapForestOfTree

          -- ** Reducing
        , reduceTree
        , reduceForest

        , keysOfTree
        , sizeOfTree

          -- ** Filtering
        , filterTree
        , filterTreesOfForest
        , filterForestsOfTree
        
          -- ** Slicing
        , sliceTree
        , sliceTreeWithNames

          -- ** Grouping
        , groupForest

          -- ** Gathering
        , gatherTree

          -- ** Traversal
        , traverseTree

          -- ** Limiting
        , Initial (..)
        , Final   (..)
        , Sample  (..))
where
import Datum.Data.Tree.Operator
import Datum.Data.Tree.Check
import Datum.Data.Tree.Exp
