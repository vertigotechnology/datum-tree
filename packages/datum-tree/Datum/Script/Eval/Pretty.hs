{-# LANGUAGE OverloadedStrings #-}
module Datum.Script.Eval.Pretty where
import Datum.Script.Eval.State
import Datum.Script.Eval.Env
import Datum.Script.Core.Exp
import Data.Monoid
import Data.Default
import Data.Text.Lazy                                   (Text)
import qualified Datum.Data.Tree.Codec.Matryo.Encode    as Matryo

import Data.Text.Lazy.Builder                   
        (Builder, fromText, toLazyText, fromString)


-- | What format to use when pretty printing trees.
data TreeFormat
        = TreeFormatInternal
        | TreeFormatSExp
        | TreeFormatMatryo 
        deriving Show

data Config
        = Config
        { configTreeFormat      :: TreeFormat }


-------------------------------------------------------------------------------
-- | Pretty print a control as lazy text.
pprControl :: Config -> Control -> Text
pprControl c cc   = toLazyText $ buildControl c cc


-------------------------------------------------------------------------------
-- | Pretty print the machine control as a bytestring.
buildControl :: Config -> Control -> Builder
buildControl c cc
 = case cc of
        ControlPAP p    -> buildPAP c p
        ControlExp x    -> buildExp c 0 x


-------------------------------------------------------------------------------
buildPAP   :: Config -> PAP -> Builder
buildPAP c (PAP p [])
        = buildPrim c p

buildPAP _c (PAP _p _vs)
        = fromText "<closure>"


-------------------------------------------------------------------------------
buildExp :: Config -> Int -> Exp -> Builder
buildExp c prec xx
 = case xx of
        XAnnot _ x      -> buildExp   c prec x
        XPrim  p        -> buildPrim  c p
        XVar   u        -> buildBound c u
        XCast  _ x      -> buildExp   c prec x

        XAbs   b t x    
         -> fromString "λ"
                <> buildBind c b 
                <> fromString ": "
                <> buildExp  c 0 t
                <> fromString "→"
                <> buildExp  c 0 x

        XApp   x1 x2
         -> buildExp c 10 x1
         <> buildExp c 0  x2

        XRec{} 
         -> error "buildPrim: handle letrec"


buildBind  :: Config -> Bind -> Builder
buildBind _cc b
 = case b of
        BAnon   -> fromString "^"
        BName n -> fromText n
        _       -> fromString "?"


buildBound :: Config -> Bound -> Builder
buildBound _cc u
 = case u of
        UIx i   -> fromString "^" <> (fromString $ show i)
        UName n -> fromText n
        _       -> fromString "?"


buildPrim  :: Config -> Prim -> Builder
buildPrim c p
 = case p of
        -- Universal.
        PHole{}         -> fromString "?"
        PType i         -> fromString "Type" <> (fromString $ show i)
        PFun{}          -> fromString "(→)"

        PAll i k b
         -> fromString "(∀"
         <> fromString (show i)
         <> buildExp c 10 k
         <> buildExp c 10 b
         <> fromString ")"

        -- Kinds
        PKComp          -> fromString "Comp"
        PKData          -> fromString "Data"
        PKAtom          -> fromString "Atom"

        -- Types
        PTS             -> fromString "S"
        PTList          -> fromString "List"
        PTName          -> fromString "Name"
        PTNum           -> fromString "Num"
        PTForest        -> fromString "Forest"
        PTTree          -> fromString "Tree"
        PTTreePath      -> fromString "TreePath"
        PTFilePath      -> fromString "FilePath"
        PTAtom at       -> buildAtomType c at

        -- Values
        PVName n        -> fromText n
        PVList{}        -> error "buildPrim: list"
        PVForest{}      -> error "buildPrim: forest"

        PVTree t       
         -> case configTreeFormat c of
                TreeFormatInternal      -> error "buildPrim: internal"
                TreeFormatSExp          -> error "buildPrim: sexp"
                TreeFormatMatryo        -> Matryo.encodeTree def t

        PVTreePath{}    -> error "buildPrim: tree path"
        PVFilePath p'   -> fromString $ show p'
        PVAtom a        -> buildAtom   c a
        PVOp op         -> buildPrimOp c op


buildAtomType :: Config -> AtomType -> Builder
buildAtomType _c at
 = case at of
        ATUnit          -> fromString "Unit"
        ATBool          -> fromString "Bool"
        ATInt           -> fromString "Int"
        ATFloat         -> fromString "Float"
        ATNat           -> fromString "Nat"
        ATDecimal       -> fromString "Decimal"
        ATText          -> fromString "Text"
        ATTime          -> fromString "Time"


buildAtom    :: Config -> Atom -> Builder
buildAtom _c aa
 = case aa of
        AUnit           -> fromString "()"
        ABool  b        -> fromString (show b)
        AInt   i        -> fromString (show i)
        AFloat f        -> fromString (show f)
        ANat   i        -> fromString (show i)
        ADecimal f      -> fromString (show f)
        AText  s        -> fromString (show s)
        ATime  t        -> fromString (show t)


buildPrimOp  :: Config -> PrimOp -> Builder
buildPrimOp _ op 
 = case op of
        PPNeg           -> fromString "neg#"
        PPAdd           -> fromString "add#"
        PPSub           -> fromString "sub#"
        PPMul           -> fromString "mul#"
        PPDiv           -> fromString "div#"
        PPEq            -> fromString "eq#"
        PPGt            -> fromString "gt#"
        PPGe            -> fromString "ge#"
        PPLt            -> fromString "lt#"
        PPLe            -> fromString "le#"
        PPArgument      -> fromString "argument#"
        PPLoad          -> fromString "load#"
        PPStore         -> fromString "store#"
        PPInitial       -> fromString "initial#"
        PPFinal         -> fromString "final#"
        PPSample        -> fromString "sample#"
        PPGroup         -> fromString "group#"
        PPGather        -> fromString "gather#"
        PPFlatten       -> fromString "flatten#"
        PPRenameFields  -> fromString "rename-fields#"
        PPPermuteFields -> fromString "permute-fields#"
        PPAt            -> fromString "at#"
        PPOn            -> fromString "on#"


