
module Datum.Script.Exp.Core where
import Datum.Script.Exp.Generic
import Datum.Data.Tree.Exp
import Data.Text                (Text)
import qualified Data.Text      as Text


---------------------------------------------------------------------------------------------------
-- | Tag for the core lanugage with a unit annotation.
data Core       = Core

type instance GXAnnot Core = ()
type instance GXPrim  Core = Prim
type instance GXBind  Core = Bind
type instance GXBound Core = Bound

type Exp        = GExp Core


---------------------------------------------------------------------------------------------------
-- | Binding occurrences of variables.
data Bind
        -- | An anonymous binder.
        = BAnon

        -- | A named binder.
        | BName String

deriving instance Show Bind
deriving instance Eq   Bind


-- | Bound occurrences of variables.
data Bound
        -- | A debruijn index.
        = UIx   Int

        -- | A named variable.
        | UName String

deriving instance Show Bound
deriving instance Eq   Bound


---------------------------------------------------------------------------------------------------
-- | Primitive object in the core language.
data Prim
        -- Universal
        = PType Int                     -- ^ Type of types at the given level.
        | PFun  Int                     -- ^ Function arrow.

        -- Kinds  (level 2)
        | PKComp                        -- ^ Kind of computation types.
        | PKData                        -- ^ Kind of data types.
        | PKAtom                        -- ^ Kind of atom types.

        -- Types  (level 1)
        | PTS                           -- ^ State computation constructor.

        | PTTree                        -- ^ Datum tree type.
        | PTTreePath                    -- ^ Datum tree path type.
        | PTFilePath                    -- ^ FilePath type.

        | PTAtom AtomType               -- ^ Atom types.
 
        -- Values (level 0)
        | PTreePath     [Text]          -- ^ A datum tree path.
        | PFilePath     FilePath        -- ^ A file path.

        | PAtom  Atom                   -- ^ Atoms.

        | PLoad                         -- ^ Load  a value from the file system.
        | PStore                        -- ^ Store a value to the file system.
        | PGather                       -- ^ Gather branches of a tree into sub trees.

deriving instance Show Prim
deriving instance Eq   Prim

pattern XType n         = XPrim (PType n)
pattern XFun  n a b     = XApp (XApp (XPrim (PFun n)) a) b

pattern XTS a           = XApp (XPrim PTS) a

pattern XTTree          = XPrim (PTTree)
pattern XTTreePath      = XPrim PTTreePath
pattern XTFilePath      = XPrim PTFilePath

pattern XTUnit          = XPrim (PTAtom ATUnit)
pattern XTBool          = XPrim (PTAtom ATBool)
pattern XTInt           = XPrim (PTAtom ATInt)
pattern XTFloat         = XPrim (PTAtom ATFloat)
pattern XTNat           = XPrim (PTAtom ATNat)
pattern XTDecimal       = XPrim (PTAtom ATDecimal)
pattern XTText          = XPrim (PTAtom ATText)
pattern XTTime          = XPrim (PTAtom ATTime)

(~>) a b  = XApp (XApp (XPrim (PFun 1)) a) b
infixr ~>

(~~>) a b = XApp (XApp (XPrim (PFun 2)) a) b
infixr ~~>


---------------------------------------------------------------------------------------------------
-- | Yield the type of the given primitive.
typeOfPrim :: Prim -> Exp
typeOfPrim pp
 = case pp of
        -- Generic
        PType n 
         -> XType (n + 1)

        PFun  n         
         -> let n'      = n + 1
            in  XFun n' (XType n') (XFun n' (XType n') (XType n'))

        -- Types of Kinds
        PKComp          -> XType 2
        PKData          -> XType 2
        PKAtom          -> XType 2

        -- Types of Types
        PTS             -> XType 1 ~~> XType 1

        PTTree          -> XType 1
        PTTreePath      -> XType 1
        PTFilePath      -> XType 1

        PTAtom _        -> XType 1

        -- Types of Values
        PTreePath  _    -> XTTreePath
        PFilePath  _    -> XTFilePath

        PAtom a         -> XPrim (PTAtom (typeOfAtom a))

        PLoad           -> XTFilePath ~> XTS XTTree
        PStore          -> XTFilePath ~> XTTree ~> XTS XTUnit
        PGather         -> XTTreePath ~> XTTree ~> XTTree


-- | Yield the type of the given atom.
typeOfAtom :: Atom -> AtomType
typeOfAtom aa
 = case aa of
        AUnit{}         -> ATUnit
        ABool{}         -> ATBool
        AInt{}          -> ATInt
        AFloat{}        -> ATFloat
        ANat{}          -> ATNat
        ADecimal{}      -> ATDecimal
        AText{}         -> ATText
        ATime{}         -> ATTime

