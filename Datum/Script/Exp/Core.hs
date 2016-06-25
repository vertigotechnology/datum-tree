
module Datum.Script.Exp.Core 
        ( Core (..), Exp
        , GExp (..)

        , GXAnnot, GXPrim
        , GXBound, GXBind
        , GXCast

        , Name
        , Bind (..), Bound (..)
        , Prim (..), Cast (..)
        , T.AtomType    (..)
        , T.Atom        (..)

        , (~>), (~~>)

        , pattern XType
        , pattern XFun

          -- types
        , pattern XTS
        , pattern XTList
        , pattern XTName
        , pattern XTTree
        , pattern XTTreePath
        , pattern XTFilePath
        , pattern XTUnit
        , pattern XTBool
        , pattern XTInt
        , pattern XTFloat
        , pattern XTNat
        , pattern XTDecimal
        , pattern XTText
        , pattern XTTime

          -- values
        , pattern XName
        , pattern XList
        , pattern XTree
        , pattern XTreePath
        , pattern XFilePath

        , pattern XUnit
        , pattern XBool
        , pattern XInt
        , pattern XFloat
        , pattern XNat
        , pattern XDecimal
        , pattern XText
        , pattern XTime

        , pattern XLoad
        , pattern XStore
        , pattern XGather
        , pattern XRenameFields

        , typeOfPrim
        , typeOfAtom)
where
import Datum.Script.Exp.Generic
import qualified Datum.Data.Tree.Exp    as T
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
-- | Tag for the core lanugage with a unit annotation.
data Core       = Core
type Exp        = GExp Core

type instance GXAnnot Core = ()
type instance GXPrim  Core = Prim
type instance GXBind  Core = Bind
type instance GXBound Core = Bound
type instance GXCast  Core = Cast


---------------------------------------------------------------------------------------------------
-- | Names of variables.
type Name
        = Text

-- | Binding occurrences of variables.
data Bind
        -- | An anonymous binder.
        = BAnon

        -- | A named binder.
        | BName Name

deriving instance Show Bind
deriving instance Eq   Bind


-- | Bound occurrences of variables.
data Bound
        -- | A debruijn index.
        = UIx   Int

        -- | A named variable.
        | UName Name

deriving instance Show Bound
deriving instance Eq   Bound


-- | Type casts.
data Cast
        -- | Run an effectful computation.
        = CRun

        -- | Box up an effectful expression.
        | CBox

deriving instance Show Cast
deriving instance Eq   Cast


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
        | PTList                        -- ^ List type constructor.

        | PTName                        -- ^ Name type.
        | PTTree                        -- ^ Datum tree type.
        | PTTreePath                    -- ^ Datum tree path type.
        | PTFilePath                    -- ^ FilePath type.

        | PTAtom    T.AtomType          -- ^ Atom types.
 
        -- Values (level 0)
        | PName     Text                -- ^ Field or branch name.
        | PList     Exp [Exp]           -- ^ List of elements of the given type.
        | PTree     (T.Tree 'T.O)       -- ^ A checked datum tree.
        | PTreePath [Text]              -- ^ A datum tree path.
        | PFilePath FilePath            -- ^ A file path.

        | PAtom      T.Atom             -- ^ Atoms.

        | PLoad                         -- ^ Load  a value from the file system.
        | PStore                        -- ^ Store a value to the file system.
        | PGather                       -- ^ Gather branches of a tree into sub trees.
        | PRenameFields                 -- ^ Rename fields of key.

deriving instance Show Prim

-- Generic
pattern XType n         = XPrim (PType n)
pattern XFun  n a b     = XApp (XApp (XPrim (PFun n)) a) b

-- Types
pattern XTS a           = XApp (XPrim PTS) a
pattern XTList a        = XApp (XPrim PTList) a

pattern XTName          = XPrim PTName
pattern XTTree          = XPrim PTTree
pattern XTTreePath      = XPrim PTTreePath
pattern XTFilePath      = XPrim PTFilePath

pattern XTUnit          = XPrim (PTAtom T.ATUnit)
pattern XTBool          = XPrim (PTAtom T.ATBool)
pattern XTInt           = XPrim (PTAtom T.ATInt)
pattern XTFloat         = XPrim (PTAtom T.ATFloat)
pattern XTNat           = XPrim (PTAtom T.ATNat)
pattern XTDecimal       = XPrim (PTAtom T.ATDecimal)
pattern XTText          = XPrim (PTAtom T.ATText)
pattern XTTime          = XPrim (PTAtom T.ATTime)

-- Values
pattern XName     n     = XPrim (PName     n)
pattern XList     t xs  = XPrim (PList     t xs)
pattern XTree     t     = XPrim (PTree     t)
pattern XTreePath ts    = XPrim (PTreePath ts)
pattern XFilePath fp    = XPrim (PFilePath fp)

pattern XUnit           = XPrim (PAtom T.AUnit)
pattern XBool     x     = XPrim (PAtom (T.ABool    x))
pattern XInt      x     = XPrim (PAtom (T.AInt     x))
pattern XFloat    x     = XPrim (PAtom (T.AFloat   x))
pattern XNat      x     = XPrim (PAtom (T.ANat     x))
pattern XDecimal  x     = XPrim (PAtom (T.ADecimal x))
pattern XText     x     = XPrim (PAtom (T.AText    x))
pattern XTime     x     = XPrim (PAtom (T.ATime    x))

pattern XLoad           = XPrim PLoad
pattern XStore          = XPrim PStore
pattern XGather         = XPrim PGather
pattern XRenameFields   = XPrim PRenameFields


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
        PTList          -> XType 1 ~~> XType 1

        PTName          -> XType 1
        PTTree          -> XType 1
        PTTreePath      -> XType 1
        PTFilePath      -> XType 1

        PTAtom _        -> XType 1

        -- Types of Values
        PName _         -> XTName
        PList t _       -> XTList t
        PTree  _        -> XTTree
        PTreePath  _    -> XTTreePath
        PFilePath  _    -> XTFilePath

        PAtom a         -> XPrim (PTAtom (typeOfAtom a))

        PLoad           -> XTFilePath ~> XTS XTTree
        PStore          -> XTFilePath ~> XTTree ~> XTS XTUnit
        PGather         -> XTTreePath ~> XTTree ~> XTTree
        PRenameFields   -> XTList XTName ~> XTTree ~> XTTree


-- | Yield the type of the given atom.
typeOfAtom :: T.Atom -> T.AtomType
typeOfAtom aa
 = case aa of
        T.AUnit{}       -> T.ATUnit
        T.ABool{}       -> T.ATBool
        T.AInt{}        -> T.ATInt
        T.AFloat{}      -> T.ATFloat
        T.ANat{}        -> T.ATNat
        T.ADecimal{}    -> T.ATDecimal
        T.AText{}       -> T.ATText
        T.ATime{}       -> T.ATTime

