{-# OPTIONS_HADDOCK hide #-}
module Datum.Data.Tree.SExp.Pretty where
import Datum.Data.Tree.Compounds
import Datum.Data.Tree.Exp

import Text.PrettyPrint.Leijen
import Data.Maybe
import Prelude                          hiding ((<$>))
import Data.Repa.Array                  (Array)
import qualified Data.Repa.Array        as A
import qualified Data.List              as L

ssym n          = parens $ text n
sexp n d        = parens $ text n <+> d


-------------------------------------------------------------------------------
-- | Pretty printer configuration.
data Config
        = Config
        { -- Desired column widths, in characters.
          configColumnFormats   :: Maybe [ColumnFormat] }



instance Monoid Config where
 mempty = Config
        { configColumnFormats   = Nothing }

 mappend 
        (Config mcw1) 
        (Config mcw2)
  =     (Config (listToMaybe $ catMaybes [mcw1, mcw2]))


-- | Combine two configs, taking the maximum of corresponding column lengths.
maxConfig :: Config -> Config -> Config
maxConfig c1 c2
 = c1   { configColumnFormats
                = maxMaybeFormats (configColumnFormats c1)
                                  (configColumnFormats c2) }
 where
        maxMaybeFormats mf1 mf2
         = case (mf1, mf2) of
                (Nothing, _)       -> mf2
                (_, Nothing)       -> mf1
                (Just f1, Just f2) -> Just $ zipWith lubColumnFormats f1 f2


-------------------------------------------------------------------------------
-- | Pretty printer format for a column.
data ColumnFormat
        = ColumnUnknown 

        -- | Column of textual values.
        | ColumnText    (Maybe Int)

        -- | Column of numeric values. 
        | ColumnNumeric (Maybe Int)
        deriving Show


lubColumnFormats :: ColumnFormat -> ColumnFormat -> ColumnFormat
lubColumnFormats f1 f2
 = case (f1, f2) of
        (ColumnUnknown,     _)
         -> f2

        (_,                ColumnUnknown)
         -> f1

        (ColumnText    mi1, ColumnText    mi2) 
         -> ColumnText    (lubMaybeInt mi1 mi2)

        (ColumnNumeric mi1, ColumnNumeric mi2) 
         -> ColumnNumeric (lubMaybeInt mi1 mi2)

        (ColumnText    mi1, ColumnNumeric mi2)
         -> ColumnText    (lubMaybeInt mi1 mi2)

        (ColumnNumeric mi1, ColumnText    mi2)
         -> ColumnText    (lubMaybeInt mi1 mi2)


lubMaybeInt :: Maybe Int -> Maybe Int -> Maybe Int
lubMaybeInt m1 m2
 = case (m1, m2) of
        (Nothing, _)       -> m2
        (_, Nothing)       -> m1
        (Just i1, Just i2) -> Just (max i1 i2)


-- Trees ----------------------------------------------------------------------
-- | Pretty print a a checked tree using S-expression syntax.
-- 
--   * To display an unchecked tree, split it into the branch and branch type,
--     then print those separately.
--
ppTree :: Config -> Tree 'O -> Doc
ppTree config (Tree b bt)
        =   sexp "tree"
        $   line
        <>  ppBranchType bt
        <$> ppBranch     config b


-- | Pretty print a checked forest using S-expression syntax.
--
--   * To display an unchecked tree, split it into the branch and branch type,
--     then print those seprately.
--
ppForest :: Config -> Forest 'O -> Doc
ppForest config (Forest g bt)
        =   sexp "forest"
        $   line
        <>  ppBranchType bt
        <$> ppGroup      config g


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
ppBranch :: Config -> Branch -> Doc
ppBranch config (B t gs)
        | A.length gs == 0
        =   ppTuple config t

        | otherwise
        =   sexp "branch"  
        $   ppTuple config t 
        <>  (nest 8 $ line <> vsep (map (ppGroup config) $ unboxes gs))


-- | Slurp a default pretty printer config for a branch.
configForBranch :: Branch -> Config
configForBranch (B t _)
        = configForTuple t


-- Group ----------------------------------------------------------------------
-- | Pretty print a `Group` using S-expression syntax.
ppGroup :: Config -> Group -> Doc
ppGroup config g@(G None bs)
        | A.length bs == 0
        = ssym "group"

        | A.length bs == 1
        , [Box b]   <- A.toList bs
        = ppBranch config b

        | otherwise
        = let   config' = configForGroup g
          in    parens  $   text "group" 
                        <$> vsep (map (ppBranch config') $ unboxes bs)

ppGroup _config g@(G (Some n) bs)
        | A.length bs == 0
        = parens $ text "group" <+> text (show n)

        | otherwise
        = let   config' = configForGroup g
          in    parens  $   text "group" <+> text (show n) 
                        <$> vsep (map (ppBranch config') $ unboxes bs)


-- | Slurp a default pretty printer config for a group.
configForGroup :: Group -> Config
configForGroup (G _ bs)
        | all isLeafBranch $ unboxes bs
        = L.foldl' maxConfig mempty 
        $ map configForBranch 
        $ unboxes bs

        | otherwise
        = mempty


-- Keys -----------------------------------------------------------------------
-- | Pretty print a `Key` using S-expression syntax.
ppKey :: Key 'O -> Doc
ppKey (Key t tt)
        = sexp "key" $ ppTuple mempty  t <+> ppTupleType tt

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
         <+> text "=" <+> ppAtomWithFormat ColumnUnknown atom


-- Tuples ---------------------------------------------------------------------
-- | Pretty print a `TupleType` using S-expression syntax.
ppTupleType :: TupleType -> Doc
ppTupleType (TT nts)
        | A.length nts == 0
        = ssym "ttype"

        | otherwise
        = sexp "ttype " 
        $ nest 8 
        $ vsep (map ppElementType $ A.toList nts)


ppElementType :: Box Name :*: Box AtomType -> Doc
ppElementType (Box n :*: Box t)
        = sexp "telement" $ text (show n) <+> ppAtomType t


-- | Pretty print a `Tuple` using S-expression syntax.
ppTuple :: Config -> Tuple -> Doc
ppTuple config (T as)
        | A.length as == 0
        = ssym "tuple"

        | Just fs <- configColumnFormats config
        = sexp "tuple" 
        $ hsep  [ ppAtomWithFormat f a
                        | a <- unboxes as
                        | f <- fs ++ repeat ColumnUnknown]

        | otherwise
        = sexp "tuple" $ (hsep $ map (ppAtomWithFormat ColumnUnknown) $ unboxes as)


-- Slurp a default pretty printer config for a tuple.
configForTuple :: Tuple -> Config
configForTuple (T as)
        = mempty
        { configColumnFormats 
                = Just  $ map formatForAtom $ unboxes as }


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

ppAtom :: Atom -> Doc
ppAtom a = ppAtomWithFormat ColumnUnknown a

-- | Pretty print an `Atom` using S-expression syntax.
ppAtomWithFormat :: ColumnFormat -> Atom -> Doc
ppAtomWithFormat f aa
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
         |  ColumnNumeric  (Just n) <- f
         -> sexp "nat"     (padL (n - 6) (int i))

         |  otherwise
         -> sexp "nat"     (int i)

        ADecimal d 
         -> sexp "decimal" (text $ show d)

        AText str
         |  ColumnText     (Just n) <- f
         -> sexp "text"    (padR (n - 7) (text $ show str))

         |  otherwise
         -> sexp "text"    (text $ show str)

        ATime str
         -> sexp "time"    (text $ show str)


-- | Get the default format for an atom.
formatForAtom :: Atom -> ColumnFormat
formatForAtom a
 = let ml       = Just $ length $ show $ ppAtomWithFormat ColumnUnknown a
   in case a of
        AUnit           -> ColumnText    ml
        ABool    _      -> ColumnText    ml
        AInt     _      -> ColumnNumeric ml
        AFloat   _      -> ColumnNumeric ml
        ANat     _      -> ColumnNumeric ml
        ADecimal _      -> ColumnNumeric ml
        AText    _      -> ColumnText    ml
        ATime    _      -> ColumnNumeric ml


-- | Pretty print an element.
ppElement :: Element c -> Doc
ppElement (Element a at)
 = parens $   text "element"
          <+> (hsep [ppAtom a, ppAtomType at])


-- Names ----------------------------------------------------------------------
ppName :: Name -> Doc
ppName n = text n


-- Paths ----------------------------------------------------------------------
ppPath :: Path -> Doc
ppPath (Path ixs _ixts)
 = parens $   text "path"
          <+> (hsep $ map ppIx ixs)

ppIx :: Ix -> Doc
ppIx ix
 = case ix of
        IField  n       -> parens $ text "ifield"  <+> (text $ show n)
        ITree   t       -> parens $ text "itree"   <+> ppTuple mempty t
        IForest n       -> parens $ text "iforest" <+> (text $ show n)


-------------------------------------------------------------------------------
padL :: Int -> Doc -> Doc
padL n doc
        =  text (replicate (max 0 (n - (length $ show doc))) ' ')
        <> doc


padR :: Int -> Doc -> Doc
padR n doc
        =  doc
        <> text (replicate (max 0 (n - (length $ show doc))) ' ')

