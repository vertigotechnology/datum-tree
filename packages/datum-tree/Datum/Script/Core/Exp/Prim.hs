{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Core.Exp.Prim 
        ( GCPrim   (..)
        , PrimOp   (..)
        , PrimData (..)
        , GExpStd
        , typeOfPrim,   typeOfAtom,     typeOfPrimOp
        , arityOfPrim,  arityOfPrimOp

        -- * Prim Ops
        , namesOfPrimOps
        , primOpsOfNames

        -- * Pattern Synonyms
        , pattern XTList,       pattern XList
        , pattern XTName,       pattern XName
        , pattern XTForest,     pattern XForest
        , pattern XTTree,       pattern XTree
        , pattern XTTreePath,   pattern XTreePath
        , pattern XTFilePath,   pattern XFilePath
        , pattern XTValue
        , pattern XTTuple

        , pattern XTBool,       pattern XBool
        , pattern XTInt,        pattern XInt
        , pattern XTFloat,      pattern XFloat
        , pattern XTNat,        pattern XNat
        , pattern XTDecimal,    pattern XDecimal
        , pattern XTText,       pattern XText
        , pattern XTTime,       pattern XTime

        , pattern XPrimOp)

where
import Datum.Script.Core.Exp.Prim.PrimOp
import Datum.Script.Core.Exp.Prim.PrimData
import Datum.Script.Kernel.Exp.Generic
import qualified Datum.Script.Kernel.Exp.Prim           as K
import qualified Datum.Script.Kernel.Exp.Compounds      as K
import qualified Datum.Script.Kernel.Exp.Bind           as K
import qualified Datum.Data.Tree.Exp                    as T


---------------------------------------------------------------------------------------------------
-- | Primitive objects in the core language.
data GCPrim x
        -- Kinds  (level 2)
        = PKAtom                        -- ^ Kind of atom types.

        -- Types  (level 1)
        | PTList                        -- ^ List type constructor.

        | PTName                        -- ^ Name type.
        | PTNum                         -- ^ Supertype of number types.

        | PTForest                      -- ^ Datum forest type.
        | PTTree                        -- ^ Datum tree type.
        | PTTreePath                    -- ^ Datum tree path type.
        | PTTuple                       -- ^ Super type of tuple types.

        | PTFilePath                    -- ^ File path type.

        | PTValue                       -- ^ Super type of values.
        | PTAtom     !T.AtomType        -- ^ Atom types.
 
        -- Values (level 0)
        | PVAtom     !T.Atom            -- ^ Atomic Values.
        | PVData     !(PrimData x)      -- ^ Primitive structured data.
        | PVOp       !PrimOp            -- ^ Primitive operators with the given type arguments.


deriving instance Show x => Show (GCPrim x)

type GExpStd l n
 =      ( GXPrim l  ~ K.GPrim (GExp l)
        , GXFrag l  ~ GCPrim  (GExp l)
        , GXBind l  ~ K.Bind  n
        , GXBound l ~ K.Bound n)


---------------------------------------------------------------------------------------------------
-- | Yield the type of the given primitive.
typeOfPrim 
        :: GExpStd l n
        => GCPrim (GExp l)
        -> GExp   l

typeOfPrim pp
 = case pp of
        -- Types of Kinds
        PKAtom          -> K.XType 2

        -- Types of Types
        PTNum           -> K.XType 1
        PTList          -> K.XType 1 ~~> K.XType 1

        PTName          -> K.XType 1

        PTForest        -> K.XType 1
        PTTree          -> K.XType 1
        PTTreePath      -> K.XType 1

        PTFilePath      -> K.XType 1
        PTTuple         -> K.XType 1
        PTValue         -> K.XType 1

        PTAtom _        -> K.XType 1

        -- Types of Values
        PVAtom a        -> XFrag (PTAtom (typeOfAtom a))
        PVData d        -> typeOfPrimData d
        PVOp   op       -> typeOfPrimOp   op


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


-- | Yield the type of the given data value.
typeOfPrimData :: GExpStd l n => PrimData (GExp l) -> GExp l
typeOfPrimData dd
 = case dd of
        PDName{}        -> XTName
        PDList t _      -> XTList t
        PDForest{}      -> XTForest
        PDTree{}        -> XTTree
        PDTreePath{}    -> XTTreePath
        PDFilePath{}    -> XTFilePath


-- | Yield the type of the given primop.
typeOfPrimOp   :: GExpStd l n => PrimOp -> GExp l
typeOfPrimOp op
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

        PPAppend        -> XTForest      ~> XTForest  ~> XTForest
        PPAt            -> XTList XTName ~> (XTTree   ~> XTTree)   ~> XTTree ~> XTTree
        PPArgument      -> XTText        ~> K.XTS XTText
        PPConcat        -> XTList XTForest ~> XTForest
        PPFinal         -> XTNat         ~> XTTree ~> XTTree
        PPFlatten       -> XTTree        ~> XTTree
        PPGather        -> XTTreePath    ~> XTTree ~> XTTree
        PPGroup         -> XTName        ~> XTTree ~> XTTree
        PPInitial       -> XTNat         ~> XTTree ~> XTTree
        PPLoad          -> XTFilePath    ~> K.XTS XTTree
        PPOn            -> XTList XTName ~> (XTForest ~> XTForest) ~> XTTree ~> XTTree
        PPPermuteFields -> XTList XTName ~> XTTree ~> XTTree
        PPRenameFields  -> XTList XTName ~> XTTree ~> XTTree
        PPSample        -> XTNat         ~> XTTree ~> XTTree
        PPSortByField   -> XTName ~> XTForest ~> XTForest
        PPStore         -> XTFilePath    ~> XTTree ~> K.XTS K.XTUnit

        PPPrint
         -> K.makeXForall K.XKData K.XKData 
                $ \u -> u ~> K.XTS K.XTUnit


-- | Yield the arity of a primitive.
arityOfPrim :: GCPrim x -> Int
arityOfPrim pp
 = case pp of
        PVOp op         -> arityOfPrimOp op
        _               -> 0


---------------------------------------------------------------------------------------------------
-- Types
pattern XTList a        = XApp (XFrag PTList) a

pattern XTName          = XFrag PTName
pattern XTForest        = XFrag PTForest
pattern XTTree          = XFrag PTTree
pattern XTTreePath      = XFrag PTTreePath
pattern XTFilePath      = XFrag PTFilePath
pattern XTValue         = XFrag PTValue
pattern XTTuple         = XFrag PTTuple

pattern XTBool          = XFrag (PTAtom T.ATBool)
pattern XTInt           = XFrag (PTAtom T.ATInt)
pattern XTFloat         = XFrag (PTAtom T.ATFloat)
pattern XTNat           = XFrag (PTAtom T.ATNat)
pattern XTDecimal       = XFrag (PTAtom T.ATDecimal)
pattern XTText          = XFrag (PTAtom T.ATText)
pattern XTTime          = XFrag (PTAtom T.ATTime)

-- Values
pattern XName     n     = XFrag (PVData (PDName     n))
pattern XList     t xs  = XFrag (PVData (PDList     t xs))
pattern XForest   f     = XFrag (PVData (PDForest   f))
pattern XTree     t     = XFrag (PVData (PDTree     t))
pattern XTreePath ts    = XFrag (PVData (PDTreePath ts))
pattern XFilePath fp    = XFrag (PVData (PDFilePath fp))

pattern XBool     x     = XFrag (PVAtom (T.ABool    x))
pattern XInt      x     = XFrag (PVAtom (T.AInt     x))
pattern XFloat    x     = XFrag (PVAtom (T.AFloat   x))
pattern XNat      x     = XFrag (PVAtom (T.ANat     x))
pattern XDecimal  x     = XFrag (PVAtom (T.ADecimal x))
pattern XText     x     = XFrag (PVAtom (T.AText    x))
pattern XTime     x     = XFrag (PVAtom (T.ATime    x))

pattern XPrimOp   p     = XFrag (PVOp p)


-- Infix constructors
(~>)    ::  (GXPrim l ~ K.GPrim (GExp l))
        =>  GExp l -> GExp l -> GExp l
(~>) a b  = XApp (XApp (XPrim (K.PFun 1)) a) b
infixr ~>


(~~>)   ::  (GXPrim l ~ K.GPrim (GExp l))
        =>  GExp l -> GExp l -> GExp l
(~~>) a b = XApp (XApp (XPrim (K.PFun 2)) a) b
infixr ~~>
