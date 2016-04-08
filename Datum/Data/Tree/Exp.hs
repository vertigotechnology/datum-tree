
module Datum.Data.Tree.Exp
        ( -- * Objects
          Checked       (..)
        , Tree          (..)
        , Forest        (..)
        , Key           (..)

          -- * Meta-Data
        , Name
        , BranchType    (..)
        , TupleType     (..)
        , AtomType      (..)

          -- * Data
        , Group         (..)
        , Branch        (..)
        , Tuple         (..)
        , Atom          (..)

          -- * Paths
        , PathType      (..)
        , Path          (..)
        , IxType        (..)
        , Ix            (..))
where


-- Objects ----------------------------------------------------------------------------------------
-- | Indication of whether an object's data has been checked against
--   the constraints in its meta-data.
--
data Checked
        = X     -- ^ Object has not been checked.
        | O     -- ^ Object has been checked.
        deriving (Show, Eq, Ord)


-- | A tree contains both branch data and branch meta-data.
--
--   * The type constructor has a phantom parameter indicating
--     whether the data has been checked against the meta-data.
--
--   * By using the raw constructor to build a tree you promise that
--     the `Checked` parameter is valid.
--
data Tree   (c :: Checked)
        = Tree  Branch 
                BranchType
        deriving Show


-- | A forest contains a sequence of trees of the same type.
--
--   * The type constructor has a phantom parameter indicating 
--     whether the data has been checked against the meta-data.
--
--   * By using the raw constructor to build a forest you promise that
--     the `Checked` parameter is valid.
--
data Forest (c :: Checked)
        = Forest 
                Group
                BranchType
        deriving Show


-- | A key contains tuple data and tuple meta-data.
--
--   * The type constructor has a phantom parameter indicating 
--     whether the data has been checked against the meta-data.
--
--   * By using the raw constructor to build a ket you promise that
--     the `Checked` parameter is valid.
--
data Key (c :: Checked)
        = Key   Tuple
                TupleType
        deriving Show


-- Meta-data --------------------------------------------------------------------------------------
-- | Branch type describes the structure of a branch.
data BranchType
        = BT    Name            -- Name of this dimension.
                TupleType       -- Tuple type.
                [BranchType]    -- Sub dimensions.
        deriving Show


-- | Named tuple types.
data TupleType
        = TT    [(Name, AtomType)]
        deriving Show


-- | Atom types.
data AtomType
        = ATUnit
        | ATBool
        | ATInt
        | ATFloat
        | ATNat
        | ATDecimal
        | ATText
        | ATTime
        deriving Show


-- Data -------------------------------------------------------------------------------------------
-- | A group with a name and list of branches.
--  
--   * The name here is not strictly required by the representation as
--     in a typed tree the group name should match the corresponding
--     branch type name. However, including it makes it easier to debug 
--     problems situtations where the tree is not well typed.
--
data Group
        = G     (Maybe Name)
                [Branch]
        deriving Show


-- | Branch with a key and forests of sub-branches.
data Branch
        = B     Tuple 
                [Group]
        deriving Show 


-- | Tuple values.
data Tuple
        = T     [Atom]
        deriving Show


-- | Atomic values.
data Atom
        = AUnit
        | ABool         Bool
        | AInt          Int
        | AFloat        Double
        | ANat          Int
        | ADecimal      Double
        | AText         String
        | ATime         String
        deriving Show


-- Names ------------------------------------------------------------------------------------------
-- | Branch and field names.
type Name
        = String


-- Path -------------------------------------------------------------------------------------------
data PathType
        = PathType  [IxType]
        deriving Show


-- | Path to a particular element.
data Path
        = Path [Ix] [IxType]
        deriving Show


-- | Type of a path.
data IxType
        = ITField       AtomType        -- When entering a field,  find out atom type.
        | ITTree        TupleType       -- When entering a field,  find out the tuple type.
        | ITForest      BranchType      -- When entering a field,  find out the branch type.
        deriving Show   

data Ix
        = IField        Name            -- When entering a field,  find out the field name.
        | ITree         Tuple           -- When entering a tree,   find out its key.
        | IForest       Name            -- When entering a forest, find out its name
        deriving Show


instance Monoid Path where
 mempty = Path [] []
 mappend (Path ps1 pts1)    (Path ps2 pts2)
        = Path (ps1 ++ ps2) (pts1 ++ pts2)


