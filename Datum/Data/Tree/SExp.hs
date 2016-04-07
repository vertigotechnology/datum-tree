
-- | The datum tree builder notation allows trees to be expressed using
--   S-expression style syntax directly in Haskell. This same S-expression
--   syntax is also used as the external representation of trees, which
--   allows data to be easilly moved between source code and external data
--   files.
--
--   The symbols, like `tree` and `branch` are encoded as Haskell functions 
--   which have types that allow them to be applied to a varying number of
--   arguments. For example so for example, you can construct 2-tuples,
--   3-tuples and 4-tuples with the same 'tuple' function, in a uniform syntax.
--
module Datum.Data.Tree.SExp 
(       -- * Tree Objects
        -- | Tree objects package up data and meta-data into the same value,
        --   and can be checked for well-formedness.
          tree
        , ppTree

        -- * Tree Types
        -- | Tree types are meta-data describing how the data is named
        --   and structured.

        -- ** Branch Types
        , tbranch,      MakeBranchType
        , ppBranch

        -- ** Tuple Types
        , ttuple
        , telement,     MakeTupleType


        -- * Tree data
        -- | Tree data consists of branches, branch groups, tuples and atoms.

        -- ** Branches
        , branch,       MakeBranch

        -- ** Branch Groups
        , group,        MakeGroup

        -- ** Tuples
        , tuple,        MakeTuple

        -- * Atoms
        -- ** Unit
        , tunit,        unit

        -- ** Bool
        , tbool,        true,   false

        -- ** Nat
        , tnat,         nat

        -- ** Int
        , tint,         int

        -- ** Float
        , tfloat,       float

        -- ** Text
        , ttext,        text

        -- ** Time
        , ttime,        time)
where
import Datum.Data.Tree.SExp.Pretty
import Datum.Data.Tree.Exp


-- Trees ----------------------------------------------------------------------
-- | Construct a tree from a branch type, and branch data.
--
-- @
-- :{  
-- (tree  (tbranch \"root\"
--                 (ttuple  (telement \"name\" ttext))
--                 (tbranch \"pets\"
--                          (ttuple  (telement \"species\" ttext)
--                                   (telement \"sort\"    ttext)))
--                 (tbranch \"dinosaurs\"
--                          (ttuple  (telement \"species\" ttext)
--                                   (telement \"size\"    ttext)
--                                   (telement \"flies\"   tbool)))
--                          (tbranch \"eats\"
--                                   (ttuple (telement \"name\"    ttext))))
--        (branch  (tuple   (text \"Pets Version 1.0\"))
--                 (group   \"pets\"
--                          (branch (tuple (text \"Dachshund\") (text \"Dog\")))
--                          (branch (tuple (text \"Saimese\")   (text \"Cat\")))
--                          (branch (tuple (text \"Gourami\")   (text \"Fish\"))))
--                 (group   \"dinosaurs\"
--                          (branch (tuple (text \"Stegosaurus\")
--                                         (text \"Enormous\")  (bool false))
--                                  (group \"eats\" 
--                                         (branch (tuple (text \"Plants\")))))
--                          (branch (tuple (text \"Pterodactyl\")
--                                         (text \"Huge\")      (bool true))
--                                  (group \"eats\"
--                                         (branch (tuple (text \"Dachshund\")))
--                                         (branch (tuple (text \"Gourami\"))))))))
--   :: Tree
-- :}
-- @
--
tree :: BranchType -> Branch -> Tree
tree bt t = Tree t bt

-- Branch Types ---------------------------------------------------------------
-- | Construct a branch type from a name, a tuple type,
--   and a possibly empty sequence of more branch types.
--  
-- @
-- :{
-- (tbranch "person" 
--          (ttuple  (telement "name" ttext) 
--                   (telement "age"  tfloat))
--          (tbranch "contact"  (ttuple (telement "number" ttext)))
--          (tbranch "project"  (ttuple (telement "name"   ttext))))
--   :: BranchType 
-- :}
-- @
--
tbranch :: MakeBranchType b => Name -> TupleType -> b
tbranch b tt = makeBranchType b tt []


class MakeBranchType a where
 makeBranchType :: Name -> TupleType -> [BranchType] -> a

instance MakeBranchType BranchType where
 makeBranchType n tt bts
        = BT n tt bts

instance (b ~ BranchType, MakeBranchType a)
      => MakeBranchType (b -> a) where
 makeBranchType n tt bts 
        = \bt -> makeBranchType n tt (bts ++ [bt])


-- Tuple Types ----------------------------------------------------------------
-- | Contruct a tuple type from a possibly empty sequence of named elements.
--
-- @
-- (ttuple) :: TupleType
--
-- (ttuple (telement "name" ttext) (telement "value" tfloat)) :: TupleType
-- @
--
ttuple :: MakeTupleType b => b
ttuple = makeTupleType []


-- | Construct an element type from an element name and an atom type.
--
-- @(telement "name" tnat)@
--
telement :: Name -> AtomType -> (Name, AtomType)
telement n at = (n, at)


class MakeTupleType a where
 makeTupleType :: [(Name, AtomType)] -> a

instance MakeTupleType TupleType where
 makeTupleType nas  = TT nas

instance (b ~ (Name, AtomType), MakeTupleType a)
      => MakeTupleType (b -> a) where
 makeTupleType nas  = \na -> makeTupleType (nas ++ [na])


-- Branches -------------------------------------------------------------------
-- | Construct a branch from a tuple, and a sequence of branch groups.
--
--   Branches may be automatically promoted to singleton groups.
--  
-- @
-- :{
-- (branch  (tuple (text \"Max\"))
--          (group "contact"
--                 (branch (tuple (text "work") (text "0400111222")))
--                 (branch (tuple (text "home") (text "0411222333"))))
--          (group "occupation"
--                 (branch (tuple (text "Data Prophet")))))
--   :: Branch
-- :}
--
-- :{
-- (branch (tuple (text "home") (text "0411222333")))
--   :: Group
-- :} 
-- @
--
branch  :: MakeBranch a => Tuple -> a
branch t = makeBranch t []


class MakeBranch a where
 makeBranch :: Tuple -> [Group] -> a

instance MakeBranch Branch where
 makeBranch t gs  = B t gs

instance MakeBranch Group where
 makeBranch t gs  = G [B t gs]

instance (b ~ Group, MakeBranch a)
      => MakeBranch (b -> a) where
 makeBranch t gs  = \g -> makeBranch t (gs ++ [g])


-- Groups ---------------------------------------------------------------------
-- | Construct a branch group from a group name and a sequence of branches.
--
-- @
-- :{
-- (group "imaginary"
--        (branch (tuple (text \"Unicorn\"))))
--    :: Group
-- :}
--
-- :{
-- (group "pets"
--        (branch (tuple (text \"Dog\")  (text \"Dachshund\")))
--        (branch (tuple (text \"Cat\")  (text \"Saimese\")))
--        (branch (tuple (text \"Fish\") (text \"Gourami\"))))
--    :: Group
-- :}
-- @
--
--
group :: MakeGroup a => String -> a
group _  = makeGroup []


class MakeGroup a where
 makeGroup :: [Branch] -> a

instance MakeGroup Group where
 makeGroup bs   = G bs

instance (b ~ Branch, MakeGroup a) 
      => MakeGroup (b -> a) where
 makeGroup bs   = \b -> makeGroup (bs ++ [b])


-- Tuples ---------------------------------------------------------------------
-- | Construct a tuple from a possibly empty sequence of atoms.
--
--   Tuples may be automatically promoted to branches and singleton groups.
-- 
-- @
-- (tuple) :: Tuple
-- (tuple (text \"Iced\") (float 3.50)) :: Tuple
-- (tuple (text \"GOOG\") (float 105.00) (nat 15609) (text \"X\")) :: Tuple
--
-- (tuple (text \"Matilda\") (text \"Hedgehog\")    (nat 36000)) :: Branch
-- (tuple (text \"Marian\")  (text \"Stegosaurus\") (nat 36000)) :: Group
-- @
--
tuple   :: MakeTuple b => b
tuple   = makeTuple []


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
-- tint    :: AtomType
tint    = ATInt

int     :: Int  -> Atom
int     = AInt


-- Float
-- tfloat  :: AtomType
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
