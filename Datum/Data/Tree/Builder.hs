module Datum.Data.Tree.Builder where
import Datum.Data.Tree.Exp


-- Trees ----------------------------------------------------------------------
tree :: String -> BranchType -> Branch -> Tree
tree _ bt t = Tree t bt


-- Branch Types ---------------------------------------------------------------
class MakeBranchType a where
 makeBranchType :: Name -> TupleType -> [BranchType] -> a

instance MakeBranchType BranchType where
 makeBranchType n tt bts
        = BT n tt bts

instance (b ~ BranchType, MakeBranchType a)
      => MakeBranchType (b -> a) where
 makeBranchType n tt bts 
        = \bt -> makeBranchType n tt (bts ++ [bt])

tbranch :: MakeBranchType b => Name -> TupleType -> b
tbranch b tt = makeBranchType b tt []


-- Tuple Types ----------------------------------------------------------------
class MakeTupleType a where
 makeTupleType :: [(Name, AtomType)] -> a

instance MakeTupleType TupleType where
 makeTupleType nas  = TT nas

instance (b ~ (Name, AtomType), MakeTupleType a)
      => MakeTupleType (b -> a) where
 makeTupleType nas  = \na -> makeTupleType (nas ++ [na])

ttuple :: MakeTupleType b => b
ttuple = makeTupleType []

telement :: Name -> AtomType -> (Name, AtomType)
telement n at = (n, at)


-- Branches -------------------------------------------------------------------
class MakeBranch a where
 makeBranch :: Tuple -> [Group] -> a

instance MakeBranch Branch where
 makeBranch t gs  = B t gs

instance MakeBranch Group where
 makeBranch t gs  = G [B t gs]

instance (b ~ Group, MakeBranch a)
      => MakeBranch (b -> a) where
 makeBranch t gs  = \g -> makeBranch t (gs ++ [g])

branch  :: MakeBranch a => Tuple -> a
branch t = makeBranch t []


class MakeGroup a where
 makeGroup :: [Branch] -> a

instance MakeGroup Group where
 makeGroup bs   = G bs

instance (b ~ Branch, MakeGroup a) 
      => MakeGroup (b -> a) where
 makeGroup bs   = \b -> makeGroup (bs ++ [b])


-- | Construct a group of branches.
group :: MakeGroup a => String -> a
group _  = makeGroup []



-- Tuples ---------------------------------------------------------------------
class MakeTuple b where
 makeTuple :: [Atom] -> b

instance MakeTuple Tuple where
 makeTuple as   = T as

instance MakeTuple Branch where
 makeTuple as   = B (T as) []

instance MakeTuple Group where
 makeTuple as   = G [B (T as) []]

instance (b ~ Atom, MakeTuple a) 
      => MakeTuple (b -> a) where
 makeTuple as   = \a -> makeTuple (as ++ [a])

tuple   :: MakeTuple b => b
tuple   = makeTuple []


-- Atoms and Atom Types--------------------------------------------------------
-- Unit
tunit   :: AtomType
tunit   = ATUnit

unit    :: Atom
unit    = AUnit


-- Bool
tbool   :: AtomType
tbool   = ATBool

bool    :: Bool -> Atom
bool    = ABool

true    = True
false   = False


-- Int
tint    :: AtomType
tint    = ATInt

int     :: Int  -> Atom
int     = AInt


-- Float
tfloat  :: AtomType
tfloat  = ATFloat

float   :: Double -> Atom
float   = AFloat


-- Nat
tnat    :: AtomType
tnat    = ATNat

nat     :: Int  -> Atom
nat     = ANat


-- Decimal
tdecimal :: AtomType
tdecimal    = ATDecimal

decimal  :: Double -> Atom
decimal  = ADecimal


-- Text
ttext   :: AtomType
ttext   = ATText

text    :: String -> Atom
text    = AText


-- Time
ttime   :: AtomType
ttime   = ATTime

time    :: String -> Atom
time    = ATime


-- Names ----------------------------------------------------------------------
name s  = s
