
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

          -- * Type Checking
        , Checked (..)
        , checkTree
        , checkForest
        , checkKey
        , Error   (..)

          -- * Operators
          -- ** Projections
        , nameOfTree
        , nameOfForest

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

          -- * Traversal
        , traverseTree

          -- * Limiting
        , Initial (..)
        , Final   (..)
        , Sample  (..))
where
import Datum.Data.Tree.Operators 
import Datum.Data.Tree.Check
import Datum.Data.Tree.Exp
