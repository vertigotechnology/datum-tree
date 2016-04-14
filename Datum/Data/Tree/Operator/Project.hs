
module Datum.Data.Tree.Operator.Project
        ( HasName (..)
        , HasMeta (..)
        , HasData (..))
where
import Datum.Data.Tree.Exp


-------------------------------------------------------------------------------
class HasName a where
 -- | Take the name of an object.
 takeName :: a -> Name


instance HasName (Tree c) where
 takeName (Tree _ bt)   = takeName bt
 {-# INLINE takeName #-}


instance HasName (Forest c) where
 takeName (Forest _ bt) = takeName bt
 {-# INLINE takeName #-}


instance HasName BranchType where
 takeName (BT n _ _)    = n
 {-# INLINE takeName #-}


-------------------------------------------------------------------------------
class HasMeta a where
 type Meta a
 -- | Take the meta-data of a typed object.
 takeMeta :: a -> Meta a


instance HasMeta (Tree c) where
 type Meta (Tree c)     = BranchType
 takeMeta (Tree _ bt)   = bt
 {-# INLINE takeMeta #-}


instance HasMeta (Forest c) where
 type Meta (Forest c)   = BranchType
 takeMeta (Forest _ bt) = bt
 {-# INLINE takeMeta #-}


instance HasMeta (Key c) where
 type Meta (Key c)      = TupleType
 takeMeta (Key _ tt)    = tt
 {-# INLINE takeMeta #-}


-------------------------------------------------------------------------------
class HasData a where
 type Data a
 -- | Take the data of a typed object.
 takeData :: a -> Data a


instance HasData (Tree c) where
 type Data (Tree c)     = Branch
 takeData  (Tree b _)   = b
 {-# INLINE takeData #-}


instance HasData (Forest c) where
 type Data (Forest c)   = Group
 takeData  (Forest g _) = g
 {-# INLINE takeData #-}


instance HasData (Key c) where
 type Data (Key c)      = Tuple
 takeData  (Key t _)    = t
 {-# INLINE takeData #-}

