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
          configColumnWidths    :: Maybe [Int] }


instance Monoid Config where
 mempty = Config
        { configColumnWidths     = Nothing }

 mappend 
        (Config mcw1) 
        (Config mcw2)
  =     (Config (listToMaybe $ catMaybes [mcw1, mcw2]))


-- | Combine two configs, taking the maximum of corresponding column lengths.
maxConfig :: Config -> Config -> Config
maxConfig c1 c2
 = c1   { configColumnWidths     
                = maxWidths (configColumnWidths c1)
                            (configColumnWidths c2) }
 where
        maxWidths Nothing mcw2          = mcw2
        maxWidths mcw1    Nothing       = mcw1
        maxWidths (Just cw1) (Just cw2) = Just (zipWith max cw1 cw2)


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

ppGroup config g@(G (Some n) bs)
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
         <+> text "=" <+> ppAtom     atom


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

        | Just nsWidth <- configColumnWidths config
        = sexp "tuple" 
        $ hsep  $ [ let str     = show $ ppAtom a
                    in  text (padR n str)
                        | a <- unboxes as
                        | n <- nsWidth ++ [0..]]

        | otherwise
        = sexp "tuple" $ (hsep $ map ppAtom $ unboxes as)

-- Slurp a default pretty printer config for a tuple.
configForTuple :: Tuple -> Config
configForTuple (T as)
 = let  lens    = map (length . show . ppAtom) $ unboxes as
   in   mempty  { configColumnWidths = Just lens }


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
        ITree   t       -> parens $ text "itree"   <+> ppTuple mempty t
        IForest n       -> parens $ text "iforest" <+> (text $ show n)


-------------------------------------------------------------------------------
{-
padL :: Int -> String -> String
padL n str
        =  replicate (max 0 (n - length str)) ' '
        ++ str
-}

padR :: Int -> String -> String
padR n str
        =  str
        ++ replicate (max 0 (n - length str)) ' '
