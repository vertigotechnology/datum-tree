
module Datum.Schema.Type 
        ( Path          (..)
        , Ix            (..)
        , BranchType    (..)
        , TupleType     (..)
        , AtomType      (..)
        , Name) 
where

-- | Path to a particular dimension.
type Path 
        = [Ix]

data Ix
        = IxSub   Name
        | IxField Name
        | IxElem  Int
        deriving Show


-- | Dimensions.
data BranchType
        = BT    Name            -- Name of this dimension.
                TupleType       -- Tuple type.
                [BranchType]    -- Sub dimensions.
        deriving Show


-- | Named tuple types.
data TupleType
        = TT [(Name, AtomType)]
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


-- | Field and dimension names.
type Name
        = String


