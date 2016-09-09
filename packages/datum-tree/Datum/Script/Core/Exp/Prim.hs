{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Core.Exp.Prim 
        ( GCPrim    (..)
        , PrimData  (..)
        , PrimField (..)
        , GExpStd
        , typeOfPrim,   typeOfAtom,     typeOfPrimOp
        , arityOfPrim,  arityOfPrimOp

        -- * Prim Types
        , PrimType  (..)
        , namesOfPrimTypes
        , primTypesOfNames

        -- * Prim Ops
        , PrimOp    (..)
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
import Datum.Script.Core.Exp.Prim.PrimType
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
        | PTType     !PrimType
 
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
        PTType{}        -> K.XType 1

        -- Types of Values
        PVData d        -> typeOfPrimData d
        PVOp   op       -> typeOfPrimOp   op


-- | Yield the type of the given data value.
typeOfPrimData :: GExpStd l n => PrimData (GExp l) -> GExp l
typeOfPrimData dd
 = case dd of
        PDType _        -> K.XType 2
        PDName{}        -> XTName
        PDAtom a        -> XFrag (PTType (PTAtom (typeOfAtom a)))
        PDTreePath{}    -> XTTreePath
        PDFilePath{}    -> XTFilePath
        PDRecord _      -> XTRecord
        PDArray t _     -> XTArray t
        PDTree{}        -> XTTree
        PDForest{}      -> XTForest


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
        T.ADate{}       -> T.ATDate


-- | Yield the type of the given primop.
typeOfPrimOp   :: GExpStd l n => PrimOp -> GExp l
typeOfPrimOp op
 = case op of
        PPNeg                   -> error "typeOfOp: finish me"
        PPAdd                   -> error "typeOfOp: finish me"
        PPSub                   -> error "typeOfOp: finish me"
        PPMul                   -> error "typeOfOp: finish me"
        PPDiv                   -> error "typeOfOp: finish me"
        PPEq                    -> error "typeOfOp: finish me"
        PPGt                    -> error "typeOfOp: finish me"
        PPGe                    -> error "typeOfOp: finish me"
        PPLt                    -> error "typeOfOp: finish me"
        PPLe                    -> error "typeOfOp: finish me"

        PPArrayEmpty            -> XTArray XTValue
        PPArrayExtend           -> XTArray XTValue ~> XTValue ~> XTArray XTValue

        PPRecordEmpty           -> XTRecord
        PPRecordExtend          -> XTRecord         ~> XTName ~> XTValue ~> XTRecord
        PPRecordProject         -> XTRecord         ~> XTName ~> XTValue

        PPLoad                  -> XTFilePath       ~> K.XTS XTTree
        PPStore                 -> XTFilePath    ~> XTTree ~> K.XTS K.XTUnit
        PPRead                  -> XTRecord         ~> XTFilePath ~> K.XTS XTTree

        PPAppend                -> XTForest         ~> XTForest  ~> XTForest
        PPAt                    -> XTArray XTName   ~> (XTTree   ~> XTTree)   ~> XTTree ~> XTTree
        PPArgument              -> XTText           ~> K.XTS XTText
        PPConcat                -> XTArray XTForest ~> XTForest
        PPCountAsField          -> XTName           ~> XTName ~> XTTree ~> XTTree
        PPDropDim               -> XTName           ~> XTTree ~> XTTree
        PPDupDim                -> XTName ~> XTName ~> XTTree ~> XTTree
        PPFinal                 -> XTNat            ~> XTTree ~> XTTree
        PPFlatten               -> XTTree           ~> XTTree
        PPGather                -> XTTreePath       ~> XTTree ~> XTTree
        PPGroup                 -> XTName           ~> XTTree ~> XTTree
        PPInitial               -> XTNat            ~> XTTree ~> XTTree
        PPMapKeys               -> (XTRecord ~> XTRecord) ~> XTForest ~> XTForest
        PPOn                    -> XTArray XTName   ~> (XTForest ~> XTForest) ~> XTTree ~> XTTree
        PPPermuteFields         -> XTArray XTName   ~> XTTree ~> XTTree
        PPRenameFields          -> XTArray XTName   ~> XTTree ~> XTTree
        PPRenameDimension       -> XTTreePath       ~> XTTreePath ~> XTTree ~> XTTree
        PPSample                -> XTNat            ~> XTTree ~> XTTree
        PPSortByField           -> XTName ~> XTForest ~> XTForest

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
pattern XTArray a       = XApp (XFrag (PTType PTArray)) a
pattern XTRecord        = XFrag (PTType PTRecord)

pattern XTName          = XFrag (PTType PTName)
pattern XTForest        = XFrag (PTType PTForest)
pattern XTTree          = XFrag (PTType PTTree)
pattern XTTreePath      = XFrag (PTType PTTreePath)
pattern XTFilePath      = XFrag (PTType PTFilePath)
pattern XTValue         = XFrag (PTType PTValue)

pattern XTBool          = XFrag (PTType (PTAtom T.ATBool))
pattern XTInt           = XFrag (PTType (PTAtom T.ATInt))
pattern XTFloat         = XFrag (PTType (PTAtom T.ATFloat))
pattern XTNat           = XFrag (PTType (PTAtom T.ATNat))
pattern XTDecimal       = XFrag (PTType (PTAtom T.ATDecimal))
pattern XTText          = XFrag (PTType (PTAtom T.ATText))
pattern XTTime          = XFrag (PTType (PTAtom T.ATTime))

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

