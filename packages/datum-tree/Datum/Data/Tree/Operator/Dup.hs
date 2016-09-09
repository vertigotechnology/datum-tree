
module Datum.Data.Tree.Operator.Dup
        ( dupDimOfTree
        , dupDimOfForest)
where
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Cast
import Datum.Data.Tree.Exp


-- | Duplicate the named sub-dimension of a tree.
dupDimOfTree :: Name -> Name -> Tree 'O -> Tree 'O
dupDimOfTree nDimSrc nDimDst 
        tree@(Tree (B t0 gs0) (BT n0 tt0 bts0))

 -- If the names match then just return the original tree.
 | nDimSrc == nDimDst
 = tree

 | otherwise
 = let  
        -- TODO: also rename any local tag on the group.
        (gs', bts')
                = unzip
                $ concat
                $ [ if n == nDimSrc
                        then [(g, bt), (g, BT nDimDst tt bts)]
                        else [(g, bt)]

                        | g                     <- unboxes gs0
                        | bt@(BT n tt bts)      <- unboxes bts0 ]

   in   Tree    (B  t0     (boxes gs'))
                (BT n0 tt0 (boxes bts'))


-- | Drop the named sub-dimension from all trees in a forest.
dupDimOfForest :: Name -> Name -> Forest 'O -> Forest 'O
dupDimOfForest nDimSrc nDimDst forest
 = promiseForest
 $ mapTreesOfForest
        (\_p tree -> dupDimOfTree nDimSrc nDimDst tree)
        mempty forest 
