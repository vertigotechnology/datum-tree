
module Datum.Schema.Pretty where
import Datum.Schema.Exp
import Text.PrettyPrint.Leijen



-- Keys -----------------------------------------------------------------------
ppKeyList :: [Key] -> Doc
ppKeyList ks
 = vsep $ map ppKey ks

ppKey :: Key -> Doc
ppKey (Key (T as) (TT nts))
 = parens $ hcat (punctuate (text ", ") (zipWith ppAT as nts))
 where  
        ppAT atom (name, ty)
         =   text name 
         <> text ":" <+> ppAtomType ty
         <+> text "=" <+> ppAtom     atom


ppTuple :: Tuple -> Doc
ppTuple (T as)
 = parens $ hcat (punctuate (text ", ") (map ppAtom as))


ppTupleType :: TupleType -> Doc
ppTupleType (TT nts)
 = parens $ hcat (punctuate (text ", ") (map ppNameType nts))
 where  ppNameType (name, ty)
         = text name <+> text ":" <+> ppAtomType ty


-- Atoms ----------------------------------------------------------------------
ppAtomType :: AtomType -> Doc
ppAtomType at
 = case at of
        ATUnit          -> text "Unit"
        ATBool          -> text "Bool"
        ATInt           -> text "Int"
        ATFloat         -> text "Float"
        ATNat           -> text "Nat"
        ATDecimal       -> text "Decimal"
        ATText          -> text "Text"
        ATTime          -> text "Time"

ppAtom :: Atom -> Doc
ppAtom aa
 = case aa of
        AUnit           -> text "()"
        ABool b         -> text $ show b
        AInt  i         -> int i
        AFloat d        -> text $ show d
        ANat  i         -> int i
        ADecimal d      -> text $ show d
        AText str       -> text $ show str
        ATime str       -> text str

