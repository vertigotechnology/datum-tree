
module Datum.Data.Tree.Operator.Flatten
        (flattenTree)
where
import Datum.Data.Tree.Compounds
import Datum.Data.Tree.Exp
import Data.Maybe
import Data.Monoid
import qualified Data.Repa.Array        as A
import qualified Data.List              as List


-- | Flatten nesting in a tree, producing a flat sequence of tuples.
flattenTree :: Tree 'O -> Tree 'O
flattenTree (Tree b0 bt0)
 = case goTree b0 bt0 of
        Nothing 
         -> emptyTree

        Just (ts0', tt0')
         -> let g       = G   None     (boxes [B t A.empty | t <- ts0'])
                -- TODO: Don't hard-code the result dimension name.
                bt      = BT  "tuples" tt0' A.empty
            in  Tree (B         mempty (A.singleton $ Box g))
                     (BT "root" mempty (A.singleton $ Box bt))

 where  
        -- Flatten a tree.
        goTree  :: Branch -> BranchType 
                -> Maybe ([Tuple], TupleType)

        goTree (B t' gs) (BT _n tt' bts)
         = let  (tsSub, ttSub)  
                        = fromMaybe ([], mempty)
                        $ merge $ catMaybes 
                        $ zipWith goForest (unboxes gs) (unboxes bts)

           in   Just    ( t'  : map (t'  <>) tsSub
                        , mergeTupleType tt' (tt' <> ttSub))

        -- Flatten a forest.
        goForest :: Group -> BranchType
                 -> Maybe ([Tuple], TupleType)

        goForest (G _n bs) bt
         = case unzip $ mapMaybe (\b -> goTree b bt) (unboxes bs) of
                (tss, tt : tts)
                  -> Just  ( concat tss
                           , List.foldl' mergeTupleType tt tts)
                _ -> Nothing


        -- Merge multuple groups of tuples.
        merge   :: [([Tuple], TupleType)] 
                ->  Maybe ([Tuple], TupleType)
        merge junk
         = case unzip junk of
                (tss, tt : tts) 
                  -> Just ( concat tss
                          , List.foldl' mergeTupleType tt tts)

                _ -> Nothing

        -- Merge tuple types.
        -- TODO: We need a sum type constructor to handle the case when 
        --       the tuple may include atoms of different types.
        -- TODO: Some tuples will also be shorter than others.
        mergeTupleType (TT atsL) (TT atsR)
         = TT (A.fromList (go (A.toList atsL) (A.toList atsR)))
         where
                go [] []
                 = []

                go (Box n :*: _ : ats1) []
                 = (Box n :*: Box ATText) : go ats1 []

                go [] ((Box n :*: _) : ats2)
                 = (Box n :*: Box ATText) : go [] ats2

                go ((Box n1 :*: _at1) : ats1) ((Box _n2 :*: _at2) : ats2)
                 = (Box n1 :*: Box ATText) : go ats1 ats2



