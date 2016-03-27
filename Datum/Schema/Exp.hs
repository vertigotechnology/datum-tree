
module Datum.Schema.Exp
        ( Path   (..)
        , Ix     (..)
        , Forest (..)
        , Tree   (..),  Branch   (..), BranchType (..)
        , Key    (..),  Tuple    (..), TupleType  (..)
        , Atom   (..),  AtomType (..)
        , Name)
where


-- Path ----------------------------------------------------------------------
-- | Path to a particular element.
type Path
        = [Ix]

data Ix
        = IxField Name
        | IxSub   Name
        | IxElem  Int
        deriving Show


-- Trees ----------------------------------------------------------------------
-- | A forest is a list of trees of the same type.
data Forest
        = Forest [Branch] BranchType
        deriving Show

-- | A tree combines the branch data and branch meta-data.
data Tree
        = Tree  Branch BranchType
        deriving Show


-- | Branch type describes the structure of a branch.
data BranchType
        = BT    Name            -- Name of this dimension.
                TupleType       -- Tuple type.
                [BranchType]    -- Sub dimensions.
        deriving Show


-- | Branch with a key and forests of sub-branches.
data Branch
        = B     Tuple [[Branch]]
        deriving Show 


-- Tuples ---------------------------------------------------------------------
-- | A key combines tuple data and tuple meta-data.
data Key
        = Key Tuple TupleType
        deriving Show

-- | Named tuple types.
data TupleType
        = TT [(Name, AtomType)]
        deriving Show


-- | Tuple values.
data Tuple
        = T [Atom]
        deriving Show


-- Atoms ----------------------------------------------------------------------
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


-- Names ----------------------------------------------------------------------
-- | Field and dimension names.
type Name
        = String
