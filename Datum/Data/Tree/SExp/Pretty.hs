{-# OPTIONS_HADDOCK hide #-}
module Datum.Data.Tree.SExp.Pretty where
import Datum.Data.Tree.Exp
import Text.PrettyPrint.Leijen
import Prelude                  hiding ((<$>))



-- Paths ----------------------------------------------------------------------
ppPath :: Path -> Doc
ppPath (Path ixs _ixts)
 = parens $   text "path"
          <+> (hsep $ map ppIx ixs)

ppIx :: Ix -> Doc
ppIx ix
 = case ix of
        IField  n       -> parens $ text "ifield"  <+> (text $ show n)
        ITree   t       -> parens $ text "itree"   <+> ppTuple t
        IForest n       -> parens $ text "iforest" <+> (text $ show n)


-- Trees ----------------------------------------------------------------------
-- | Pretty print a a checked tree using S-expression syntax.
-- 
--   * To display an unchecked tree, split it into the branch and branch type,
--     then print those separately.
--
ppTree :: Tree 'O -> Doc
ppTree (Tree b bt)
 = parens 
 $      text "tree" 
 <>     (nest 4 $   line 
                <>  (parens $ ppBranch b)
                <$> (parens $ ppBranchType bt))


-- | Pretty print a checked forest using S-expression syntax.
--
--   * To dispaly an unchecked tree, split it into the branch and branch type,
--     then print those seprately.
--
ppForest :: Forest 'O -> Doc
ppForest (Forest (G _n []) (BT name kt _))
 =      text "+ " <> text (show name) <+> ppTupleType kt

ppForest (Forest (G _n bs) bt@(BT name kt _bts))
 =      text "+ " <> text (show name) <+> ppTupleType kt
 <>     (nest 4 $ line <> vsep (map ppTree [Tree b bt | b <- bs]))


-- BranchType -----------------------------------------------------------------
-- | Pretty print a `BranchType` using S-expression syntax.
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


-- Branch ---------------------------------------------------------------------
-- | Pretty print a `Branch` using S-expression syntax.
ppBranch :: Branch -> Doc

ppBranch (B t [G _n []])
 =      text "branch" <+> ppTuple t <+> text "{}"

ppBranch (B t [])
 =      text "branch" <+> ppTuple t <+> text "{}"

ppBranch (B t subs)
 =      text "branch" <+> ppTuple t
 <>     (nest 4 $  text " {"
                <> line  
                <> (vsep (map (\bg -> ppGroup bg <> semi) subs)))
 <>     line    <> text "}"


-- Group ----------------------------------------------------------------------
-- | Pretty print a `Group` using S-expression syntax.
ppGroup :: Group -> Doc
ppGroup (G _n bs)
 =      text "group" 
 <>     (nest 4 $  text " {"
                <> line  
                <> (vsep (map (\b  -> ppBranch b <> semi) bs)))
 <>     line    <> text "}"


-- Keys -----------------------------------------------------------------------
-- | Pretty print a `Key` using S-expression syntax.
ppKey :: Key 'O -> Doc
ppKey (Key (T as) (TT nts))
 = parens $ hcat (punctuate (text ", ") (zipWith ppAT as nts))
 where  
        ppAT atom (name, ty)
         =   text name 
         <>  text ":" <+> ppAtomType ty
         <+> text "=" <+> ppAtom     atom


ppKeyList :: [Key 'O] -> Doc
ppKeyList ks
 = vsep $ map ppKey ks


ppKeyNamed :: Key 'O -> Doc
ppKeyNamed (Key (T as) (TT nts))
 = parens $ hcat (punctuate (text ", ") (zipWith ppAT as nts))
 where  
        ppAT atom (name, _)
         =   text name 
         <+> text "=" <+> ppAtom     atom


-- Tuples ---------------------------------------------------------------------
-- | Pretty print a `TupleType` using S-expression syntax.
ppTupleType :: TupleType -> Doc
ppTupleType (TT nts)
 = parens $ hcat (punctuate (text ", ") (map ppNameType nts))
 where  ppNameType (name, ty)
         = text name <> text ":" <+> ppAtomType ty


-- | Pretty print a `Tuple` using S-expression syntax.
ppTuple :: Tuple -> Doc
ppTuple (T [])
 = parens (text "tuple")

ppTuple (T as)
 = parens (text "tuple" <+> (hsep $ map ppAtom as))


-- Atoms ----------------------------------------------------------------------
-- | Pretty print an `AtomType` using S-expression syntax.
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


-- | Pretty print an `Atom` using S-expression syntax.
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

