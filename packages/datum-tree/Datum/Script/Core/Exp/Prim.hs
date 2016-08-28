{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Core.Exp.Prim 
        ( GCPrim    (..)
        , PrimOp    (..)
        , PrimData  (..)
        , PrimField (..)
        , GExpStd
        , typeOfPrim,   typeOfAtom,     typeOfPrimOp
        , arityOfPrim,  arityOfPrimOp

        -- * Prim Ops
        , namesOfPrimOps
        , primOpsOfNames

        -- * Pattern Synonyms
        , pattern XTName,       pattern XName
        , pattern XTTreePath,   pattern XTreePath
        , pattern XTFilePath,   pattern XFilePath
        , pattern XTArray,      pattern XArray
        , pattern XTTree,       pattern XTree
        , pattern XTForest,     pattern XForest
        , pattern XTValue

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
        | PTValue                       -- ^ Super type of values.
        | PTName                        -- ^ Name type.
        | PTNum                         -- ^ Supertype of number types.
        | PTAtom     !T.AtomType        -- ^ Atom types.
        | PTTreePath                    -- ^ Datum tree path type.
        | PTFilePath                    -- ^ File path type.
        | PTArray                       -- ^ Array type constructor.
        | PTRecord                      -- ^ Record type constructor.
        | PTTree                        -- ^ Datum tree type.
        | PTForest                      -- ^ Datum forest type.
 
        -- Values (level 0)
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
        PTValue         -> K.XType 1
        PTNum           -> K.XType 1
        PTName          -> K.XType 1
        PTAtom _        -> K.XType 1
        PTTreePath      -> K.XType 1
        PTFilePath      -> K.XType 1
        PTArray         -> K.XType 1
        PTRecord        -> K.XType 1
        PTTree          -> K.XType 1
        PTForest        -> K.XType 1

        -- Types of Values
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
        PDAtom a        -> XFrag (PTAtom (typeOfAtom a))
        PDTreePath{}    -> XTTreePath
        PDFilePath{}    -> XTFilePath
        PDRecord _      -> XTRecord
        PDArray t _     -> XTArray t
        PDTree{}        -> XTTree
        PDForest{}      -> XTForest


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

        PPArrayEmpty    -> XTArray XTValue
        PPArrayExtend   -> XTArray XTValue ~> XTValue ~> XTArray XTValue

        PPRecordEmpty   -> XTRecord
        PPRecordExtend  -> XTRecord         ~> XTName ~> XTValue ~> XTRecord

        PPAppend        -> XTForest         ~> XTForest  ~> XTForest
        PPAt            -> XTArray XTName   ~> (XTTree   ~> XTTree)   ~> XTTree ~> XTTree
        PPArgument      -> XTText           ~> K.XTS XTText
        PPConcat        -> XTArray XTForest ~> XTForest
        PPFinal         -> XTNat            ~> XTTree ~> XTTree
        PPFlatten       -> XTTree           ~> XTTree
        PPGather        -> XTTreePath       ~> XTTree ~> XTTree
        PPGroup         -> XTName           ~> XTTree ~> XTTree
        PPInitial       -> XTNat            ~> XTTree ~> XTTree
        PPLoad          -> XTFilePath       ~> K.XTS XTTree
        PPOn            -> XTArray XTName   ~> (XTForest ~> XTForest) ~> XTTree ~> XTTree
        PPPermuteFields -> XTArray XTName   ~> XTTree ~> XTTree
        PPRenameFields  -> XTArray XTName   ~> XTTree ~> XTTree
        PPSample        -> XTNat            ~> XTTree ~> XTTree
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
pattern XTArray a       = XApp (XFrag PTArray) a
pattern XTRecord        = XFrag PTRecord

pattern XTName          = XFrag PTName
pattern XTForest        = XFrag PTForest
pattern XTTree          = XFrag PTTree
pattern XTTreePath      = XFrag PTTreePath
pattern XTFilePath      = XFrag PTFilePath
pattern XTValue         = XFrag PTValue

pattern XTBool          = XFrag (PTAtom T.ATBool)
pattern XTInt           = XFrag (PTAtom T.ATInt)
pattern XTFloat         = XFrag (PTAtom T.ATFloat)
pattern XTNat           = XFrag (PTAtom T.ATNat)
pattern XTDecimal       = XFrag (PTAtom T.ATDecimal)
pattern XTText          = XFrag (PTAtom T.ATText)
pattern XTTime          = XFrag (PTAtom T.ATTime)

-- Values
pattern XName     n     = XFrag (PVData (PDName     n))
pattern XTreePath ts    = XFrag (PVData (PDTreePath ts))
pattern XFilePath fp    = XFrag (PVData (PDFilePath fp))
pattern XArray    t xs  = XFrag (PVData (PDArray    t xs))
pattern XTree     t     = XFrag (PVData (PDTree     t))
pattern XForest   f     = XFrag (PVData (PDForest   f))

pattern XBool     x     = XFrag (PVData (PDAtom (T.ABool    x)))
pattern XInt      x     = XFrag (PVData (PDAtom (T.AInt     x)))
pattern XFloat    x     = XFrag (PVData (PDAtom (T.AFloat   x)))
pattern XNat      x     = XFrag (PVData (PDAtom (T.ANat     x)))
pattern XDecimal  x     = XFrag (PVData (PDAtom (T.ADecimal x)))
pattern XText     x     = XFrag (PVData (PDAtom (T.AText    x)))
pattern XTime     x     = XFrag (PVData (PDAtom (T.ATime    x)))

pattern XPrimOp   p     = XFrag (PVOp p)


-- Infix constructors
(~>)    ::  (GXPrim l ~ K.GPrim (GExp l))
        =>  GExp l -> GExp l -> GExp l
(~>) a b  = XApp (XApp (XPrim (K.PFun 1)) a) b
infixr ~>

{-
(~~>)   ::  (GXPrim l ~ K.GPrim (GExp l))
        =>  GExp l -> GExp l -> GExp l
(~~>) a b = XApp (XApp (XPrim (K.PFun 2)) a) b
infixr ~~>
-}