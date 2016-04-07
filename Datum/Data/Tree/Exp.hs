
module Datum.Data.Tree.Exp
        ( Path   (..),  PathType (..)
        , Ix     (..),  IxType   (..)

        , Forest (..)

        , Tree   (..)
        , Group  (..)
        , Branch (..),  BranchType (..)

        , Key    (..)
        , Tuple  (..),  TupleType  (..)

        , Atom   (..),  AtomType (..)
        , Name)
where


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


-- Trees ------------------------------------------------------------------------------------------
-- | A forest is a list of trees of the same type.
data Forest
        = Forest 
                Group
                BranchType
        deriving Show


-- | A tree combines the branch data and branch meta-data.
data Tree
        = Tree  Branch 
                BranchType
        deriving Show


-- | Branch type describes the structure of a branch.
data BranchType
        = BT    Name            -- Name of this dimension.
                TupleType       -- Tuple type.
                [BranchType]    -- Sub dimensions.
        deriving Show

-- | A group of branches.
data Group
        = G     [Branch]
        deriving Show


-- | Branch with a key and forests of sub-branches.
data Branch
        = B     Tuple 
                [Group]
        deriving Show 


-- Tuples -----------------------------------------------------------------------------------------
-- | A key combines tuple data and tuple meta-data.
data Key
        = Key   Tuple
                TupleType
        deriving Show

-- | Named tuple types.
data TupleType
        = TT    [(Name, AtomType)]
        deriving Show


-- | Tuple values.
data Tuple
        = T     [Atom]
        deriving Show


-- Atoms ------------------------------------------------------------------------------------------
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
-- | Field and dimension names.
type Name
        = String
