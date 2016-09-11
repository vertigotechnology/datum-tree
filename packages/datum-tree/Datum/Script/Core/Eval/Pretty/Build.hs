
module Datum.Script.Core.Eval.Pretty.Build
        ( buildExp
        , buildBind
        , buildBound
        , buildPrim
        , buildFrag
        , buildPrimType
        , buildAtomType
        , buildPrimData
        , buildPrimField
        , buildAtom)
where
import Datum.Script.Core.Exp
import Data.Monoid
import Data.Default
import qualified Datum.Data.Tree.Codec.Matryo.Encode    as Matryo
import qualified Data.List                              as List
import qualified Data.Repa.Scalar.Date32                as Date32
import Data.Text.Lazy.Builder                   
        (Builder, fromText, fromString)


buildExp :: Int -> Exp -> Builder
buildExp prec xx
 = case xx of
        XAnnot _ x      -> buildExp   prec x
        XPrim  p        -> buildPrim  p
        XFrag  f        -> buildFrag  f
        XVar   u        -> buildBound u
        XCast  _ x      -> buildExp   prec x

        XAbs   b t x    
         -> fromString "λ"
                <> buildBind b 
                <> fromString ": "
                <> buildExp  0 t
                <> fromString "→"
                <> buildExp  0 x

        XApp   x1 x2
         -> buildExp 10 x1
         <> buildExp 0  x2

        XRec{} 
         -> error "buildPrim: handle letrec"


buildBind  :: Bind -> Builder
buildBind b
 = case b of
        BAnon   -> fromString "^"
        BName n -> fromText n
        _       -> fromString "?"


buildBound :: Bound -> Builder
buildBound u
 = case u of
        UIx i   -> fromString "^" <> (fromString $ show i)
        UName n -> fromText n
        _       -> fromString "?"


buildPrim  :: Prim -> Builder
buildPrim p
 = case p of
        -- Universal.
        PHole{}         -> fromString "?"
        PMeta _ i       -> fromString "!"    <> (fromString $ show i)
        PType i         -> fromString "Type" <> (fromString $ show i)
        PFun{}          -> fromString "(→)"

        PAll i k b
         -> fromString "(∀"
         <> fromString (show i)
         <> buildExp   10 k
         <> buildExp   10 b
         <> fromString ")"

        -- Kinds
        PKData          -> fromString "Data"
        PKEffect        -> fromString "Effect"

        -- Types
        PTS             -> fromString "S"
        PTVoid          -> fromString "Void"
        PTUnit          -> fromString "Unit"

        -- Values
        PVUnit          -> fromString "()"


buildFrag     :: Frag -> Builder
buildFrag f
 = case f of
        PKAtom          -> fromString "Atom"
        PTType pt       -> buildPrimType pt 
        PVData d        -> buildPrimData d
        PVOp op         -> buildPrimOp   op


buildPrimType :: PrimType -> Builder
buildPrimType pt
 = case pt of
        PTArray         -> fromString "Array"
        PTRecord        -> fromString "Record"
        PTName          -> fromString "Name"
        PTNum           -> fromString "Num"
        PTForest        -> fromString "Forest"
        PTTree          -> fromString "Tree"
        PTTreePath      -> fromString "TreePath"
        PTFilePath      -> fromString "FilePath"
        PTAtom at       -> buildAtomType at
        PTValue         -> fromString "Value"


buildAtomType :: AtomType -> Builder
buildAtomType at
 = case at of
        ATUnit          -> fromString "Unit"
        ATBool          -> fromString "Bool"
        ATInt           -> fromString "Int"
        ATFloat         -> fromString "Float"
        ATNat           -> fromString "Nat"
        ATDecimal       -> fromString "Decimal"
        ATText          -> fromString "Text"
        ATTime          -> fromString "Time"
        ATDate          -> fromString "Date"


buildPrimData :: PrimData Exp -> Builder
buildPrimData dd
 = case dd of
        PDType t        -> buildExp  0 t
        PDName n        -> fromText  n
        PDAtom a        -> buildAtom a
        PDTreePath{}    -> error "buildPrim: tree path"
        PDFilePath p'   -> fromString $ show p'

        PDRecord fs
         -> fromString "{"
         <> (mconcat
                $ List.intersperse (fromString ", ")
                $ map buildPrimField fs)
         <> fromString "}"

        PDArray _ ds
         -> fromString "["
         <> (mconcat 
                $ List.intersperse (fromString ", ") 
                $ map buildPrimData ds)
         <> fromString "]"

        PDForest f      -> Matryo.encodeForest def f
        PDTree   t      -> Matryo.encodeTree   def t


buildPrimField :: PrimField Exp -> Builder
buildPrimField (PFField n _t v)
        =  fromText n 
        <> fromString " = "
        <> buildPrimData v


buildAtom :: Atom -> Builder
buildAtom aa
 = case aa of
        AUnit           -> fromString "()"
        ABool  b        -> fromString (show b)
        AInt   i        -> fromString (show i)
        AFloat f        -> fromString (show f)
        ANat   i        -> fromString (show i)
        ADecimal f      -> fromString (show f)
        AText  s        -> fromString (show s)
        ATime  t        -> fromString (show t)

        ADate  d
         -> let (yy, mm, dd)    = Date32.unpack d
            in fromString "'"
                <> fromString (show yy) <> fromString "-"
                <> fromString (show mm) <> fromString "-"
                <> fromString (show dd)


buildPrimOp :: PrimOp -> Builder
buildPrimOp op 
 = case Prelude.lookup op namesOfPrimOps of
        -- The primops table should have all the names,
        -- if it doesn't then add the name for your new primop.
        Nothing         -> error "datum.buildPrimOp: 'namesOfPrimOps table' is inexhaustive"
        Just name       -> fromString name


