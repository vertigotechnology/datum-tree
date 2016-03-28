
module Datum.Schema.PrettyCtor where
import Datum.Schema.Operator
import Datum.Schema.Exp
import Text.PrettyPrint.Leijen
import Prelude                  hiding ((<$>))


-- Trees-----------------------------------------------------------------------
ppTree :: Tree -> Doc
ppTree (Tree b bt)
 =      text "tree" 
 <>     (nest 4 $ line 
                <>  (parens $ ppBranch b)
                <$> (parens $ ppBranchType bt))


ppForest :: Forest -> Doc
ppForest (Forest [] bt@(BT name kt _))
 =      text "+ " <> text (show name) <+> ppTupleType kt
ppForest (Forest bs bt@(BT name kt bts))
 =      text "+ " <> text (show name) <+> ppTupleType kt
 <>     (nest 4 $ line <> vsep (map ppTree [Tree b bt | b <- bs]))


ppBranchType :: BranchType -> Doc
ppBranchType (BT name tt [])
 = hsep [ text "branchtype"
        , text (show name)
        , ppTupleType tt]

ppBranchType (BT name tt bts)
 = hsep [ text "branchtype"
        , text (show name)
        , ppTupleType tt]
 <>     (nest 4 $  text " {"
                <> line 
                <> (vsep (map (\bt -> ppBranchType bt <> semi) bts)))
 <>     line    <> text "}"




ppBranch :: Branch -> Doc

ppBranch (B t [[]])
 =      text "branch" <+> ppTuple t <+> text "{}"

ppBranch (B t [])
 =      text "branch" <+> ppTuple t <+> text "{}"

ppBranch (B t subs)
 =      text "branch" <+> ppTuple t
 <>     (nest 4 $  text " {"
                <> line  
                <> (vsep (map (\bg -> ppBranchGroup bg <> semi) subs)))
 <>     line    <> text "}"


ppBranchGroup :: [Branch] -> Doc
ppBranchGroup bs
 =      text "group" 
 <>     (nest 4 $  text " {"
                <> line  
                <> (vsep (map (\b  -> ppBranch b <> semi) bs)))
 <>     line    <> text "}"




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
ppTuple (T [])
 = parens (text "tuple")

ppTuple (T as)
 = parens (text "tuple" <+> (hsep $ map ppAtom as))


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
        ATime str       -> text $ show str

