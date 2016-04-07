
module Datum.Data.Tree.Pretty.Chrome where
import Datum.Data.Tree.Operator
import Datum.Data.Tree.Exp
import Text.PrettyPrint.Leijen
import Prelude                  hiding ((<$>))


-- Trees-----------------------------------------------------------------------
ppTree :: Tree -> Doc

ppTree (Tree (B k []) (BT n _kt []))
 =      text ". " <> ppTuple k

ppTree (Tree (B k xssSub) (BT n kt tSubs))
 =      text "* " <> ppTuple k
 <$>    (vsep $ map ppForest $ zipWith makeForest xssSub tSubs)


ppForest :: Forest -> Doc

ppForest (Forest (G []) bt@(BT name kt _))
 =      text "+ " <> text name <+> text "~" <+> ppTupleType kt

ppForest (Forest (G bs) bt@(BT name kt bts))
 =      text "+ " <> text name <+> text "~" <+> ppTupleType kt
 <>     (nest 4 $ line <> vsep (map ppTree [Tree b bt | b <- bs]))


ppBranch :: Branch -> Doc
ppBranch (B t [])
 =      text ". " <> ppTuple t

ppBranch (B t [G []])
 =      text ". " <> ppTuple t

ppBranch (B t gs)
 =      text "* " <> ppTuple t
 <>     (nest 4 $ line <> vsep (map ppGroup gs))


ppGroup :: Group -> Doc
ppGroup (G bs)
 =      vsep (map ppBranch bs)


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
         <>  text ":" <+> ppAtomType ty
         <+> text "=" <+> ppAtom     atom


-- | Pretty print a key with field names, but no field types.
ppKeyNamed :: Key -> Doc
ppKeyNamed (Key (T as) (TT nts))
 = parens $ hcat (punctuate (text ", ") (zipWith ppAT as nts))
 where  
        ppAT atom (name, _)
         =   text name 
         <+> text "=" <+> ppAtom     atom


-- | Pretty print a `Tuple`.
ppTuple :: Tuple -> Doc
ppTuple (T as)
 = parens $ hcat (punctuate (text ", ") (map ppAtom as))


-- | Pretty print a `TupleType`.
ppTupleType :: TupleType -> Doc
ppTupleType (TT nts)
 = parens $ hcat (punctuate (text ", ") (map ppNameType nts))
 where  ppNameType (name, ty)
         = text name <> text ":" <+> ppAtomType ty


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

