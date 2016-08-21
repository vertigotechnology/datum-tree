
module Datum.Data.Tree.Operator.Project
        ( HasMeta (..)

        , HasData (..)

        , HasName (..)
        , hasName

        , HasKey  (..)
        , hasKey)
where
import Datum.Data.Tree.Exp.Data


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

instance HasData [Forest c] where
 type Data [Forest c]   = [Group]
 takeData ks            = map takeData ks
 {-# INLINE takeData #-}


instance HasData (Key c) where
 type Data (Key c)      = Tuple
 takeData  (Key t _)    = t
 {-# INLINE takeData #-}

instance HasData [Key c] where
 type Data [Key c]      = [Tuple]
 takeData ks            = map takeData ks
 {-# INLINE takeData #-}


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


-- | Check if this object has the given name.
hasName :: HasName a => String -> a -> Bool
hasName n x = takeName x == n


-------------------------------------------------------------------------------
class HasKey obj where
 -- | Take the key of a typed object.
 takeKey :: obj c -> Key c

instance HasKey Key  where
 takeKey k = k
 {-# INLINE takeKey #-}

instance HasKey Tree where
 takeKey (Tree (B t _) (BT _ tt _) ) = Key t tt
 {-# INLINE takeKey #-}


-- | Check if this object has the given key.
hasKey :: (HasKey obj, Eq (Key c)) => Key c -> obj c -> Bool
hasKey k o = takeKey o == k
{-# INLINE hasKey #-}

