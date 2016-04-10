{-# OPTIONS_HADDOCK hide #-}
module Datum.Data.Tree.SExp.Pretty where
import Datum.Data.Tree.Exp

import Text.PrettyPrint.Leijen
import Prelude                          hiding ((<$>))
import Data.Repa.Array                  (Array)
import qualified Data.Repa.Array        as A


ssym n          = parens $ text n
sexp n d        = parens $ text n <+> d


-- Trees ----------------------------------------------------------------------
-- | Pretty print a a checked tree using S-expression syntax.
-- 
--   * To display an unchecked tree, split it into the branch and branch type,
--     then print those separately.
--
ppTree :: Tree 'O -> Doc
ppTree (Tree b bt)
        =   sexp "tree"
        $   line
        <>  ppBranchType bt
        <$> ppBranch b


-- | Pretty print a checked forest using S-expression syntax.
--
--   * To dispaly an unchecked tree, split it into the branch and branch type,
--     then print those seprately.
--
ppForest :: Forest 'O -> Doc
ppForest (Forest g bt)
        =   sexp "forest"
        $   line
        <>  ppBranchType bt
        <$> ppGroup g


-- BranchType -----------------------------------------------------------------
-- | Pretty print a `BranchType` using S-expression syntax.
ppBranchType :: BranchType -> Doc
ppBranchType (BT name tt (bts :: Array (Box BranchType)))
        | A.length bts == 0
        =   parens
        $   text "tbranch"
        <+> text (show name)
        <>  (nest 8 $ line
                <>  ppTupleType tt)

        | otherwise
        =   parens
        $   text "tbranch"
        <+> text (show name)
        <>  (nest 8 $ line 
                <>  ppTupleType tt
                <$> vsep (map ppBranchType $ unboxes bts))


-- Branch ---------------------------------------------------------------------
-- | Pretty print a `Branch` using S-expression syntax.
ppBranch :: Branch -> Doc
ppBranch (B t gs)
        | A.length gs == 0
        = ppTuple t

        | otherwise
        =   sexp "branch"  
        $   ppTuple t 
        <>  (nest 8 $ line <> vsep (map ppGroup $ unboxes gs))


-- Group ----------------------------------------------------------------------
-- | Pretty print a `Group` using S-expression syntax.
ppGroup :: Group -> Doc

ppGroup (G None bs)
        | A.length bs == 0
        = ssym "group"

        | A.length bs == 1
        , [Box b]   <- A.toList bs
        = ppBranch b

        | otherwise
        = parens $  text "group" 
                <$> vsep (map ppBranch $ unboxes bs)

ppGroup (G (Some n) bs)
        | A.length bs == 0
        = parens $ text "group" <+> text (show n)

        | otherwise
        = parens $ text "group" <+> text (show n) 
                <$> vsep (map ppBranch $ unboxes bs)


-- Keys -----------------------------------------------------------------------
-- | Pretty print a `Key` using S-expression syntax.
ppKey :: Key 'O -> Doc
ppKey (Key t tt)
        = sexp "key" $ ppTuple t <+> ppTupleType tt

ppKeyList :: [Key 'O] -> Doc
ppKeyList ks
        = vsep $ map ppKey ks


ppKeyNamed :: Key 'O -> Doc
ppKeyNamed (Key (T as) (TT nts))
 = parens $ hcat (punctuate (text ", ") 
                 (zipWith ppAT 
                        (unboxes as)
                        [nt | nt <- A.toList nts]))
 where  
        ppAT atom (Box name :*: _)
         =   text name 
         <+> text "=" <+> ppAtom     atom


-- Tuples ---------------------------------------------------------------------
-- | Pretty print a `TupleType` using S-expression syntax.
ppTupleType :: TupleType -> Doc
ppTupleType (TT nts)
        = sexp "ttype " 
        $ nest 8 
        $ vsep (map ppElementType $ A.toList nts)


ppElementType :: Box Name :*: Box AtomType -> Doc
ppElementType (Box n :*: Box t)
        = sexp "telement" $ text (show n) <+> ppAtomType t


-- | Pretty print a `Tuple` using S-expression syntax.
ppTuple :: Tuple -> Doc
ppTuple (T as)
        | A.length as == 0
        = ssym "tuple"

        | otherwise
        = sexp "tuple" $ (hsep $ map ppAtom $ unboxes as)


-- Atoms ----------------------------------------------------------------------
-- | Pretty print an `AtomType` using S-expression syntax.
ppAtomType :: AtomType -> Doc
ppAtomType at
 = case at of
        ATUnit          -> text "tunit"
        ATBool          -> text "tbool"
        ATInt           -> text "tint"
        ATFloat         -> text "tfloat"
        ATNat           -> text "tnat"
        ATDecimal       -> text "tdecimal"
        ATText          -> text "ttext"
        ATTime          -> text "ttime"


-- | Pretty print an `Atom` using S-expression syntax.
ppAtom :: Atom -> Doc
ppAtom aa
 = case aa of
        AUnit           
         -> ssym "unit"

        ABool b
         -> sexp "bool"    (text $ show b)
 
        AInt  i         
         -> sexp "int"     (int i)

        AFloat d
         -> sexp "float"   (text $ show d)

        ANat  i
         -> sexp "nat"     (int i)

        ADecimal d 
         -> sexp "decimal" (text $ show d)

        AText str
         -> sexp "text"    (text $ show str)

        ATime str
         -> sexp "time"    (text $ show str)


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



