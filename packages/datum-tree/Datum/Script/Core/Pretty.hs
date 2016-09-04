
module Datum.Script.Core.Pretty where
import Datum.Script.Core.Exp
import Datum.Script.Kernel.Exp.Pretty
import Text.PrettyPrint.Leijen
import qualified Data.Text              as Text


-- Pretty printer dictionary.
dPpExp :: PpExp Core
dPpExp
        = PpExp
        { ppAnnot       = const empty
        , ppExp         = ppExp'   dPpExp
        , ppBind        = ppBind'  (text . Text.unpack)
        , ppBound       = ppBound' (text . Text.unpack)
        , ppCast        = ppCast'
        , ppPrim        = ppPrim'  dPpExp
        , ppFrag        = ppFrag' 
        }


ppFrag' :: GCPrim (GExp Core) -> Doc
ppFrag' pp
 = case pp of
        PKAtom          -> text "Atom"
        PTType  pt      -> ppPrimType pt
        PVData  pd      -> ppPrimData pd
        PVOp    op      -> ppPrimOp   op


ppPrimType :: PrimType -> Doc
ppPrimType _ = text "primtype"


ppPrimData :: PrimData Exp -> Doc
ppPrimData _ = text "primdata"


ppPrimOp   :: PrimOp   -> Doc
ppPrimOp _   = text "primop"

