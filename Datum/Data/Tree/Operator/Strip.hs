
module Datum.Data.Tree.Operator.Strip
        ( Strippable (..))
where
import Datum.Data.Tree.Exp


class Strippable a b where
 -- | Strip a 'b' thing from an 'a' thing.
 --
 --   The 'a' thing always contains a 'b' thing that can be stripped out.
 strip :: a -> b


-------------------------------------------------------------------------------
instance Strippable (Tree c) Branch where
 strip (Tree b _)     = b
 {-# INLINE strip #-}

instance Strippable (Tree c) Tuple where
 strip (Tree b _)     = strip b
 {-# INLINE strip #-}


instance Strippable (Tree c) BranchType where
 strip (Tree _ bt)    = bt
 {-# INLINE strip #-}

instance Strippable (Tree c) Name where
 strip (Tree _ bt)    = strip bt
 {-# INLINE strip #-}

instance Strippable (Tree c) TupleType where
 strip (Tree _ bt)    = strip bt
 {-# INLINE strip #-}


instance Strippable (Tree c) (Key c) where
 strip (Tree (B k _) (BT _ tt _))       
        = Key k tt
 {-# INLINE strip #-}


-------------------------------------------------------------------------------
instance Strippable (Forest c) Group where
 strip (Forest g _) = g
 {-# INLINE strip #-}


instance Strippable (Forest c) BranchType where
 strip (Forest _ bt) = bt
 {-# INLINE strip #-}


-------------------------------------------------------------------------------
instance Strippable BranchType Name where
 strip (BT n _ _) = n
 {-# INLINE strip #-}


instance Strippable BranchType TupleType where
 strip (BT _ tt _) = tt
 {-# INLINE strip #-}


-------------------------------------------------------------------------------
instance Strippable Branch Tuple where
 strip (B t _) = t
 {-# INLINE strip #-}

