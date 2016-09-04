{-# LANGUAGE OverloadedStrings #-}
module Datum.Script.Core.Eval.Pretty 
        ( ppExpCut
        , ppBound
        , ppBind)
where
import Datum.Script.Core.Exp
import Datum.Script.Core.Eval.Pretty.Build
import qualified Datum.Script.Kernel.Exp.Pretty         as K
import qualified Data.Text                              as Text
import qualified Data.Text.Lazy                         as LText
import qualified Data.Text.Lazy.Builder                 as LText
import Text.PrettyPrint.Leijen


-------------------------------------------------------------------------------
ppExpCut :: Exp -> Doc
ppExpCut xx     = K.ppExp dPpExp_cut xx

ppBound  :: Bound -> Doc
ppBound u       = K.ppBound' (text . Text.unpack) u

ppBind   :: Bind  -> Doc
ppBind  b       = K.ppBind'  (text . Text.unpack) b


-------------------------------------------------------------------------------
dPpExp_cut :: K.PpExp Core
dPpExp_cut  = K.PpExp
        { K.ppAnnot      = const      empty
        , K.ppExp        = K.ppExp'   dPpExp_cut
        , K.ppBind       = K.ppBind'  (text . Text.unpack)
        , K.ppBound      = K.ppBound' (text . Text.unpack)
        , K.ppCast       = K.ppCast'
        , K.ppPrim       = K.ppPrim'  dPpExp_cut
        , K.ppFrag       = ppFrag_cut
        }


ppFrag_cut :: Frag -> Doc
ppFrag_cut ff
 = case ff of
        PKAtom          -> text "Atom"
        PTType pt       -> ppPrimType'    pt
        PVData pd       -> ppPrimData_cut pd
        PVOp   op       -> ppPrimOp'      op


ppPrimType' :: PrimType -> Doc
ppPrimType' pt
        = text
        $ LText.unpack
        $ LText.toLazyText
        $ buildPrimType pt


-- | Pretty print a data value,
--   cutting the length of the output to 50 characters.
--
--   If the interpreter crashes due to a type error then we only want the 
--   first few characters of the trees in the current control, 
--   rather than dumping the whole lot to the console.
--
ppPrimData_cut :: PrimData Exp -> Doc
ppPrimData_cut pd  
 = let ss       = take 50 
                $ LText.unpack
                $ LText.toLazyText
                $ buildPrimData pd
   in  if length ss < 50
        then text ss
        else text "(" <> text ss <> text "...)"


ppPrimOp'   :: PrimOp  -> Doc
ppPrimOp' op
 = case Prelude.lookup op namesOfPrimOps of
        -- The primops table should have all the names,
        -- if it doesn't then add the name for your new primop.
        Nothing    -> error "datum.buildPrimOp: 'namesOfPrimOps table' is inexhaustive"
        Just name  -> text name

