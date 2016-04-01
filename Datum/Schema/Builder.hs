{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies  #-}

module Datum.Schema.Builder where
import Datum.Schema.Exp


-- Branches -------------------------------------------------------------------
class MakeBranch a where
 makeBranch :: Tuple -> [Group] -> a

instance MakeBranch Branch where
 makeBranch t gs  = B t gs

instance MakeBranch Group where
 makeBranch t gs  = G [B t gs]

instance (b ~ Group, MakeBranch a) => MakeBranch (b -> a) where
 makeBranch t gs  = \g -> makeBranch t (gs ++ [g])

branch  :: MakeBranch a => Tuple -> a
branch t = makeBranch t []


class MakeGroup a where
 makeGroup :: [Branch] -> a

instance MakeGroup Group where
 makeGroup bs   = G bs

instance (b ~ Branch, MakeGroup a) => MakeGroup (b -> a) where
 makeGroup bs   = \b -> makeGroup (bs ++ [b])

group :: MakeGroup a => a
group   = makeGroup []


-- Tuples ---------------------------------------------------------------------
class MakeTuple b where
 makeTuple :: [Atom] -> b

instance MakeTuple Tuple where
 makeTuple as   = T as

instance MakeTuple Branch where
 makeTuple as   = B (T as) []

instance MakeTuple Group where
 makeTuple as   = G [B (T as) []]

instance (b ~ Atom, MakeTuple a) => MakeTuple (b -> a) where
 makeTuple as     = \a -> makeTuple (as ++ [a])

tuple   :: MakeTuple b => b
tuple   = makeTuple []


-- Atoms ----------------------------------------------------------------------
-- | Construct a unit atom.
unit    :: Atom
unit    = AUnit


-- | Construct a boolean atom.
bool    :: Bool -> Atom
bool    = ABool

true    = True
false   = False


-- | Construct an integer atom.
int     :: Int  -> Atom
int     = AInt


-- | Construct a float atom.
float   :: Double -> Atom
float   = AFloat


-- | Construct a natural number atom.
nat     :: Int  -> Atom
nat     = ANat


-- | Construct a decimal number atom.
decimal :: Double -> Atom
decimal = ADecimal


-- | Construct a text atom.
text    :: String -> Atom
text    = AText


-- | Construct a time atom.
time    :: String -> Atom
time    = ATime


-- Names ----------------------------------------------------------------------
name s  = s
