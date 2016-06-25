{-# LANGUAGE UndecidableInstances #-}

module Datum.Script.Exp.Prim where
import Datum.Script.Exp.Generic
import qualified Datum.Data.Tree.Exp    as T
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
-- | Primitive objects in the core language.
data GPrim l
        -- Universal, works at all levels.
        = PType Int                     -- ^ Type of types at the given level.
        | PFun  Int                     -- ^ Function arrow at the given level.

        -- Kinds  (level 2)
        | PKComp                        -- ^ Kind of computation types.
        | PKData                        -- ^ Kind of data types.
        | PKAtom                        -- ^ Kind of atom types.

        -- Types  (level 1)
        | PTS                           -- ^ State computation type constructor.
        | PTList                        -- ^ List type constructor.

        | PTName                        -- ^ Name type.
        | PTTree                        -- ^ Datum tree type.
        | PTTreePath                    -- ^ Datum tree path type.
        | PTFilePath                    -- ^ File path type.

        | PTAtom    T.AtomType          -- ^ Atom types.
 
        -- Values (level 0)
        | PVName     Text               -- ^ Field or branch name.
        | PVList     (GExp l) [GExp l]  -- ^ List of elements of the given type.
        | PVTree     (T.Tree 'T.O)      -- ^ Checked datum tree.
        | PVTreePath [Text]             -- ^ Datum tree path.
        | PVFilePath FilePath           -- ^ File path.

        | PVAtom      T.Atom            -- ^ Atomic Values.

        | PVLoad                        -- ^ Load  a value from the file system.
        | PVStore                       -- ^ Store a value to the file system.
        | PVGather                      -- ^ Gather branches of a tree into sub trees.
        | PVRenameFields                -- ^ Rename fields of key.


deriving instance ShowGExp l => Show (GPrim l)


---------------------------------------------------------------------------------------------------
-- | Yield the type of the given primitive.
typeOfPrim :: (GPrim l ~ GXPrim l) =>  GPrim l -> GExp l
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
        PVName _        -> XTName
        PVList t _      -> XTList t
        PVTree  _       -> XTTree
        PVTreePath  _   -> XTTreePath
        PVFilePath  _   -> XTFilePath

        PVAtom a        -> XPrim (PTAtom (typeOfAtom a))

        PVLoad          -> XTFilePath ~> XTS XTTree
        PVStore         -> XTFilePath ~> XTTree ~> XTS XTUnit
        PVGather        -> XTTreePath ~> XTTree ~> XTTree
        PVRenameFields  -> XTList XTName ~> XTTree ~> XTTree


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


---------------------------------------------------------------------------------------------------
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
pattern XName     n     = XPrim (PVName     n)
pattern XList     t xs  = XPrim (PVList     t xs)
pattern XTree     t     = XPrim (PVTree     t)
pattern XTreePath ts    = XPrim (PVTreePath ts)
pattern XFilePath fp    = XPrim (PVFilePath fp)

pattern XUnit           = XPrim (PVAtom  T.AUnit)
pattern XBool     x     = XPrim (PVAtom (T.ABool    x))
pattern XInt      x     = XPrim (PVAtom (T.AInt     x))
pattern XFloat    x     = XPrim (PVAtom (T.AFloat   x))
pattern XNat      x     = XPrim (PVAtom (T.ANat     x))
pattern XDecimal  x     = XPrim (PVAtom (T.ADecimal x))
pattern XText     x     = XPrim (PVAtom (T.AText    x))
pattern XTime     x     = XPrim (PVAtom (T.ATime    x))

pattern XLoad           = XPrim PVLoad
pattern XStore          = XPrim PVStore
pattern XGather         = XPrim PVGather
pattern XRenameFields   = XPrim PVRenameFields


(~>) a b  = XApp (XApp (XPrim (PFun 1)) a) b
infixr ~>

(~~>) a b = XApp (XApp (XPrim (PFun 2)) a) b
infixr ~~>


