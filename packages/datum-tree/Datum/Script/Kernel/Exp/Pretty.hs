
module Datum.Script.Kernel.Exp.Pretty 
        ( PpExp (..)
        , ppBind'
        , ppBound'
        , ppExp'
        , ppPrim'
        , ppCast')
where
import Datum.Script.Kernel.Exp.Generic
import Datum.Script.Kernel.Exp.Bind
import Datum.Script.Kernel.Exp.Cast
import Datum.Script.Kernel.Exp.Prim
import Text.PrettyPrint.Leijen


-------------------------------------------------------------------------------
data PpExp l
        = PpExp
        { ppAnnot      :: GXAnnot l -> Doc
        , ppExp        :: GExp    l -> Doc
        , ppBind       :: GXBind  l -> Doc
        , ppBound      :: GXBound l -> Doc
        , ppCast       :: GXCast  l -> Doc
        , ppPrim       :: GXPrim  l -> Doc
        , ppFrag       :: GXFrag  l -> Doc
        }


-------------------------------------------------------------------------------
-- | Pretty print a bind.
ppBind' :: (n -> Doc)
        -> Bind n -> Doc

ppBind' ppName bb
 = case bb of
        BAnon   -> text "^"
        BName n -> ppName n
        BNone   -> text "_"


-------------------------------------------------------------------------------
-- | Pretty print a bound.
ppBound' :: (n -> Doc)
         -> Bound n -> Doc

ppBound' ppName uu
 = case uu of
        UIx i   -> text "^" <> int i
        UName n -> ppName n


-------------------------------------------------------------------------------
-- | Pretty print a cast.
ppCast' :: Cast -> Doc
ppCast' cc
 = case cc of
        CastRun -> text "run"
        CastBox -> text "box"


-------------------------------------------------------------------------------
-- | Pretty print an expression.
ppExp'  :: PpExp l -> GExp l -> Doc
ppExp'  p =  ppExp'' p ContextTop


ppExp'' p ctx xx
 = let  bparens True  d = text "(" <> d <> text ")"
        bparens False d = d

   in case xx of
        XAnnot a x
         -> ppAnnot p a <> ppExp p x

        XVar u
         -> ppBound p u

        XAbs bParam xType xBody
         -> bparens (elem ctx [ContextArg])
         $  text "λ(" 
                <>  ppBind   p bParam 
                <+> text ":"
                <+> ppExp    p xType
                <>  text ") → "
                <>  ppExp''  p ContextBody xBody

        XApp x1 x2
         ->  bparens (elem ctx [ContextArg])
         $   ppExp'' p ContextFun x1 
         <+> ppExp'' p ContextArg x2

        XRec bxs xBody
         ->  text "rec {"
         <>  vsep [ ppBind p b <+> text "=" <+> ppExp p x
                  | (b, x) <- bxs ]
         <>  text "} in "
         <>  ppExp'' p ContextBody xBody

        XCast c xBody
         ->  ppCast p c 
         <+> text "in" 
         <+> ppExp'' p ContextBody xBody

        XIf  xScrut xThen xElse
         ->  bparens (elem ctx [ContextFun, ContextArg])
         $   text "if"   <+> ppExp' p xScrut
         <+> text "then" <+> ppExp' p xThen
         <+> text "else" <+> ppExp' p xElse

        XPrim pp
         -> ppPrim p pp

        XFrag ff
         -> ppFrag p ff
 

data Context 
        = ContextArg
        | ContextFun
        | ContextBody
        | ContextTop
        deriving Eq



-------------------------------------------------------------------------------
-- | Pretty print a primitive.
ppPrim'
        :: PpExp l -> GPrim (GExp l) -> Doc


ppPrim' _p pp
 = case pp of
        PMeta{} -> text "meta"
        _       -> text "TODO"


