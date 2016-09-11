
module Datum.Data.Tree.Exp.Data where
import Data.Repa.Scalar.Box
import Data.Repa.Scalar.Option           
import Data.Repa.Scalar.Product
import Data.Hashable
import Data.Repa.Array                          (Array)
import Data.Repa.Scalar.Date32                  (Date32)
import qualified Data.Repa.Array                as A
import qualified Data.Repa.Scalar.Date32        as Date32


-- Objects --------------------------------------------------------------------
-- | Indication of whether an object's data has been checked against
--   the constraints in its meta-data.
--
data Checked
        = O     -- ^ Object has been checked.
        | X     -- ^ Object has not been checked.
        deriving (Show, Eq, Ord)

type family CheckMin (a :: Checked) (b :: Checked) where
  CheckMin 'O 'O = 'O
  CheckMin 'O 'X = 'X
  CheckMin 'X 'O = 'X
  CheckMin 'X 'X = 'X


-- | A tree contains both branch data and branch meta-data.
data Tree   (c :: Checked)
        -- | By using the raw constructor to build a tree you promise that
        --   the `Checked` parameter is valid.
        = Tree  !Branch         -- Tree data.
                !BranchType     -- Tree meta-data.
        deriving Show


-- | A forest contains a sequence of trees of the same type.
data Forest (c :: Checked)
        -- | By using the raw constructor to build a forest you promise that
        --   the `Checked` parameter is valid.
        = Forest 
                !Group          -- A group of trees of the same type.
                !BranchType     -- The shared type of the trees.
        deriving Show


-- | A key contains tuple data and tuple meta-data.
data Key (c :: Checked)
        -- | By using the raw constructor to build a key you promise that
        --   the `Checked` parameter is valid.
        = Key   !Tuple          -- Key tuple value
                !TupleType      -- Key tuple type.
        deriving Show


instance Monoid (Key c) where
 mempty = Key mempty mempty
 mappend (Key t1 tt1) (Key t2 tt2)
        = Key (mappend t1 t2) (mappend tt1 tt2)


-- | An element contains an atom and an atom type.
data Element (c :: Checked)
        -- | By using the raw constructor to build an element you promise that
        --   the `Checked` parameter is valid.
        = Element !Atom !AtomType
        deriving Show


instance Eq  (Element c) where
 (==) (Element a1 _) (Element a2 _)
  = a1 == a2

 (/=) (Element a1 _) (Element a2 _)
  = a1 /= a2


instance Ord (Element c) where
 compare (Element a1 _) (Element a2 _)
  = compare a1 a2


-- Meta-data ------------------------------------------------------------------
-- | Branch type describes the structure of a branch.
data BranchType
        = BT    !Name                           -- Name of this dimension.
                !TupleType                      -- Tuple type.
                !(Array (Box BranchType))       -- Sub dimensions.
        deriving Show


instance Eq BranchType where
 (==) (BT n1 tt1 bts1) (BT n2 tt2 bts2)
  = let bts1'   = unboxes bts1
        bts2'   = unboxes bts2
    in  n1 == n2 && tt1 == tt2 && bts1' == bts2'


-- | Named tuple types.
data TupleType
        = TT    !(Array (Box Name :*: Box AtomType))
        deriving Show


instance Eq TupleType where
 (==) (TT nas1)  (TT nas2)
  = let (ns1, ats1) = unzip [(n, t) | (Box n :*: Box t) <- A.toList nas1]
        (ns2, ats2) = unzip [(n, t) | (Box n :*: Box t) <- A.toList nas2]
    in  ns1 == ns2 && ats1 == ats2


instance Monoid TupleType where
 mempty = TT (A.empty)
 mappend (TT as1) (TT as2)
        = TT (A.fromList (A.toList as1 ++ A.toList as2))


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
        | ATDate
        deriving (Show, Eq)


-- Data -----------------------------------------------------------------------
-- | A group with a name and list of branches.
--  
--   * The name here is not strictly required by the representation as
--     in a typed tree the group name should match the corresponding
--     branch type name. However, including it makes it easier to debug 
--     problems situtations where the tree is not well typed.
--
data Group
        = G     !(Option Name)                  -- Group name.
                !(Array (Box Branch))           -- Data of trees in this group.
        deriving Show


-- | Branch with a key and forests of sub-branches.
data Branch
        = B     !Tuple                          -- Branch key tuple.
                !(Array (Box Group))            -- Sub groups of this tree.
        deriving Show 


-- | Tuple values.
data Tuple
        = T     !(Array (Box Atom))             -- Tuple field values.
        deriving Show


instance Monoid Tuple where
 mempty = T (A.empty)
 mappend (T t1) (T t2)
        = T (A.fromList (A.toList t1 ++ A.toList t2))


-- | Atomic values.
data Atom
        = AUnit
        | ABool         !Bool
        | AInt          !Int
        | AFloat        !Double
        | ANat          !Int
        | ADecimal      !Double
        | AText         !String
        | ATime         !String

        -- A date with year, month, day components.
        | ADate         !Date32
        deriving (Eq, Ord, Show)


instance Hashable Atom where
 hashWithSalt s atom
  = case atom of
        AUnit           -> hashWithSalt (s + 0) ()
        ABool    b      -> hashWithSalt (s + 1) b
        AInt     i      -> hashWithSalt (s + 2) i
        AFloat   f      -> hashWithSalt (s + 3) f
        ANat     n      -> hashWithSalt (s + 4) n
        ADecimal d      -> hashWithSalt (s + 5) d
        AText    t      -> hashWithSalt (s + 6) t
        ATime    t      -> hashWithSalt (s + 7) t

        ADate    d
         -> let (yy, mm, dd) = Date32.unpack d
            in  hashWithSalt (s + 8) (yy, mm, dd)


-- Names ----------------------------------------------------------------------
-- | Branch and field names.
type Name
        = String


-- Path -----------------------------------------------------------------------
data PathType
        = PathType  ![IxType]
        deriving Show

instance Monoid PathType where
 mempty = PathType []
 mappend (PathType pt1) (PathType pt2)
        = PathType (pt1 ++ pt2)


-- | Path to a particular element.
data Path
        = Path ![Ix] ![IxType]
        deriving Show


-- | Type of a path.
data IxType
        -- | The atom type of a field.
        = ITField       !AtomType

        -- | The tuple type of a tree.
        | ITTree        !TupleType

        -- | The branch type of a forest.
        | ITForest      !BranchType
        deriving Show   

data Ix
        -- | The field name of a field.
        = IField        !Name

        -- | The key of a tree.
        | ITree         !Tuple

        -- | The group name of a forest.
        | IForest       !Name
        deriving Show


instance Monoid Path where
 mempty = Path [] []
 mappend (Path ps1 pts1)    (Path ps2 pts2)
        = Path (ps1 ++ ps2) (pts1 ++ pts2)


-- Utils ----------------------------------------------------------------------
-- | Unpack an array of boxed things into a lazy list.
unboxes :: Array (Box a) -> [a]
unboxes arr = map unbox $ A.toList arr
{-# INLINE unboxes #-}


-- | Evaluate a a lazy list of things into an array.
boxes   :: [a] -> Array (Box a)
boxes ls    = A.fromList $ map box ls
{-# INLINE boxes #-}

