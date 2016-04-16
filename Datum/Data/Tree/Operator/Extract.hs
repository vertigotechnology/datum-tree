
module Datum.Data.Tree.Operator.Extract
        (Extractable (..))
where
import Datum.Data.Tree.Exp


class Extractable a b where
 -- | Extract a 'b' thing from an 'a' thing, if there is one.
 extract :: a -> Maybe b


instance Extractable Group Name where
 extract (G on _)
  = case on of
        None    -> Nothing
        Some x  -> Just x


instance Extractable Atom a
      => Extractable (Element c) a where
 extract (Element a _)  = extract a
 {-# INLINE extract #-}


instance Extractable Atom ()  where
 extract AUnit           = Just ()
 extract _               = Nothing
 {-# INLINE extract #-}


instance Extractable Atom Bool  where
 extract (ABool b)       = Just b
 extract _               = Nothing
 {-# INLINE extract #-}


instance Extractable Atom Int   where
 extract (AInt  i)       = Just i
 extract (ANat  i)       = Just i
 extract _               = Nothing
 {-# INLINE extract #-}


instance Extractable Atom Double where
 extract (AFloat   d)    = Just d
 extract (ADecimal d)    = Just d
 extract _               = Nothing
 {-# INLINE extract #-}


instance Extractable Atom String where
 extract (AText t)       = Just t
 extract (ATime t)       = Just t
 extract _               = Nothing
 {-# INLINE extract #-}
