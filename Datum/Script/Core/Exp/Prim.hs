{-# LANGUAGE UndecidableInstances #-}

module Datum.Script.Core.Exp.Prim where
import Datum.Script.Core.Exp.Generic
import qualified Datum.Data.Tree.Exp    as T
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
-- | Primitive objects in the core language.
data GPrim x
        -- Universal, works at all levels.
        = PHole x                       -- ^ A hole of the given type, to be elaborated.
        | PType Int                     -- ^ Type of types at the given level.
        | PFun  Int                     -- ^ Function arrow at the given level.
        | PAll  Int x x                 -- ^ Universal quantification with a kind and bound.

        -- Kinds  (level 2)
        | PKComp                        -- ^ Kind of computation types.
        | PKData                        -- ^ Kind of data types.
        | PKAtom                        -- ^ Kind of atom types.

        -- Types  (level 1)
        | PTS                           -- ^ State computation type constructor.
        | PTList                        -- ^ List type constructor.

        | PTName                        -- ^ Name type.
        | PTNum                         -- ^ Supertype of number types.

        | PTForest                      -- ^ Datum forest type.
        | PTTree                        -- ^ Datum tree type.
        | PTTreePath                    -- ^ Datum tree path type.

        | PTFilePath                    -- ^ File path type.

        | PTAtom     T.AtomType         -- ^ Atom types.
 
        -- Values (level 0)
        | PVName     Text               -- ^ Field or branch name.
        | PVList     x [x]              -- ^ List of elements of the given type.

        | PVForest   (T.Forest 'T.O)    -- ^ Checked datum forest.
        | PVTree     (T.Tree   'T.O)    -- ^ Checked datum tree.
        | PVTreePath [Text]             -- ^ Datum tree path.

        | PVFilePath FilePath           -- ^ File path.

        | PVAtom     T.Atom             -- ^ Atomic Values.
        | PVOp       PrimOp             -- ^ Primitive operators with the given type arguments.

data PrimOp
        = PPNeg                         -- ^ Negation.
        | PPAdd                         -- ^ Addition.
        | PPSub                         -- ^ Subtraction.
        | PPMul                         -- ^ Multiplication.
        | PPDiv                         -- ^ Division

        | PPEq                          -- ^ Equality.
        | PPGt                          -- ^ Greater-than.
        | PPGe                          -- ^ Greater-than-equal.
        | PPLt                          -- ^ Less-than.
        | PPLe                          -- ^ Less-than-equal.

        | PPArgument                    -- ^ Get the value of a script argument.
        | PPLoad                        -- ^ Load  a value from the file system.
        | PPStore                       -- ^ Store a value to the file system.
        | PPInitial                     -- ^ Select the initial n branches of each subtree.
        | PPFinal                       -- ^ Select the final n branches of each subtree.
        | PPSample                      -- ^ Sample n intermediate branches of each subtree.
        | PPGroup                       -- ^ Group branches by given key field.
        | PPGather                      -- ^ Gather branches of a tree into sub trees.
        | PPFlatten                     -- ^ Flatten branches.
        | PPRenameFields                -- ^ Rename fields of key.
        | PPPermuteFields               -- ^ Permute fields of a key.

        | PPAt                          -- ^ Apply a per-tree function at the given path.
        | PPOn                          -- ^ Apply a per-forest function at the given path. 

deriving instance Show x => Show (GPrim x)
deriving instance Show PrimOp


---------------------------------------------------------------------------------------------------
-- | Yield the type of the given primitive.
typeOfPrim 
        :: (GXPrim l ~ GPrim (GExp l))
        => GPrim (GExp l) -> GExp l
typeOfPrim pp
 = case pp of
        -- Generic
        PHole t         -> t

        PType n 
         -> XType (n + 1)

        PFun  n         
         -> let n'      = n + 1
            in  XFun n' (XType n') (XFun n' (XType n') (XType n'))

        PAll  n k t
         -> let n'      = n + 1
            in  XFun n' k (XFun n' t (XType n'))

        -- Types of Kinds
        PKComp          -> XType 2
        PKData          -> XType 2
        PKAtom          -> XType 2

        -- Types of Types
        PTS             -> XType 1 ~~> XType 1
        PTNum           -> XType 1
        PTList          -> XType 1 ~~> XType 1

        PTName          -> XType 1

        PTForest        -> XType 1
        PTTree          -> XType 1
        PTTreePath      -> XType 1

        PTFilePath      -> XType 1

        PTAtom _        -> XType 1

        -- Types of Values
        PVName _        -> XTName
        PVList t _      -> XTList t

        PVForest _      -> XTForest
        PVTree   _      -> XTTree
        PVTreePath  _   -> XTTreePath

        PVFilePath  _   -> XTFilePath

        PVAtom a        -> XPrim (PTAtom (typeOfAtom a))
        PVOp   op       -> typeOfOp op


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


-- | Yield the type of the given primop.
typeOfOp :: (GXPrim l ~ GPrim (GExp l))
         => PrimOp -> GExp l
typeOfOp op
 = case op of
        PPNeg           -> error "typeOfOp: finish me"
        PPAdd           -> error "typeOfOp: finish me"
        PPSub           -> error "typeOfOp: finish me"
        PPMul           -> error "typeOfOp: finish me"
        PPDiv           -> error "typeOfOp: finish me"

        PPEq            -> error "typeOfOp: finish me"
        PPGt            -> error "typeOfOp: finish me"
        PPGe            -> error "typeOfOp: finish me"
        PPLt            -> error "typeOfOp: finish me"
        PPLe            -> error "typeOfOp: finish me"

        PPArgument      -> XTText        ~> XTS XTText
        PPLoad          -> XTFilePath    ~> XTS XTTree
        PPStore         -> XTFilePath    ~> XTTree ~> XTS XTUnit
        PPInitial       -> XTNat         ~> XTTree ~> XTTree
        PPFinal         -> XTNat         ~> XTTree ~> XTTree
        PPSample        -> XTNat         ~> XTTree ~> XTTree
        PPGroup         -> XTName        ~> XTTree ~> XTTree
        PPGather        -> XTTreePath    ~> XTTree ~> XTTree
        PPFlatten       -> XTTree        ~> XTTree
        PPRenameFields  -> XTList XTName ~> XTTree ~> XTTree
        PPPermuteFields -> XTList XTName ~> XTTree ~> XTTree

        PPAt            -> XTList XTName ~> (XTTree   ~> XTTree)   ~> XTTree ~> XTTree
        PPOn            -> XTList XTName ~> (XTForest ~> XTForest) ~> XTTree ~> XTTree


-- | Yield the arity of a primitive.
arityOfPrim :: GPrim x -> Int
arityOfPrim pp
 = case pp of
        PVOp op         -> arityOfOp op
        _               -> 0


-- | Yield the arity of a primitive operator.
arityOfOp :: PrimOp -> Int
arityOfOp op
 = case op of
        PPNeg           -> 1
        PPAdd           -> 2
        PPSub           -> 2
        PPMul           -> 2
        PPDiv           -> 2

        PPEq            -> 2
        PPGt            -> 2
        PPGe            -> 2
        PPLt            -> 2
        PPLe            -> 2

        PPArgument      -> 1
        PPLoad          -> 1
        PPStore         -> 2
        PPInitial       -> 2
        PPFinal         -> 2
        PPSample        -> 2
        PPGroup         -> 2
        PPGather        -> 2
        PPFlatten       -> 1
        PPRenameFields  -> 2
        PPPermuteFields -> 2

        PPAt            -> 3
        PPOn            -> 3


---------------------------------------------------------------------------------------------------
-- Generic
pattern XType n         = XPrim (PType n)
pattern XFun  n a b     = XApp (XApp (XPrim (PFun n)) a) b

-- Types
pattern XTS a           = XApp (XPrim PTS) a
pattern XTList a        = XApp (XPrim PTList) a

pattern XTName          = XPrim PTName
pattern XTForest        = XPrim PTForest
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
pattern XForest   f     = XPrim (PVForest   f)
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

pattern XArgument       = XPrim (PVOp PPArgument)
pattern XLoad           = XPrim (PVOp PPLoad)
pattern XStore          = XPrim (PVOp PPStore)
pattern XInitial        = XPrim (PVOp PPInitial)
pattern XFinal          = XPrim (PVOp PPFinal)
pattern XSample         = XPrim (PVOp PPSample)
pattern XGroup          = XPrim (PVOp PPGroup)
pattern XGather         = XPrim (PVOp PPGather)
pattern XFlatten        = XPrim (PVOp PPFlatten)
pattern XRenameFields   = XPrim (PVOp PPRenameFields)
pattern XPermuteFields  = XPrim (PVOp PPPermuteFields)

pattern XAt             = XPrim (PVOp PPAt)
pattern XOn             = XPrim (PVOp PPOn)

(@@) a b  = XApp a b
infixl 9 @@

(~>) a b  = XApp (XApp (XPrim (PFun 1)) a) b
infixr ~>

(~~>) a b = XApp (XApp (XPrim (PFun 2)) a) b
infixr ~~>


