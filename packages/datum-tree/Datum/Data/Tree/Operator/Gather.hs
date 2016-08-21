
module Datum.Data.Tree.Operator.Gather
        (gatherTree)
where
import Datum.Data.Tree.Exp
import Data.Maybe
import Data.Monoid
import qualified Data.Repa.Array        as A


-- | Gather tuples on the given path into a new tree.
gatherTree :: [Name] -> Tree c -> Tree c
gatherTree ns0 t0
 = case collectTree [] mempty mempty t0 of
        -- No branches matched, so we don't know what the result type
        -- should be. Just return the empty tree as a place holder.
        Nothing         
         -> emptyTree

        -- We found some matching branches, so pack them into 
        -- a new tree. 
        Just (bs, bt)   
         -> Tree (B         mempty (A.singleton $ Box $ G None $ boxes bs))
                 (BT "root" mempty (A.singleton $ Box bt))
 where  
        -- Collect matching branches in a tree.
        collectTree ns t tt (Tree (B t' gs) (BT n tt' bts))
         | ns0 == ns ++ [n]
         = let  bs'     = [B    (t  <> t')  gs]
                bt'     =  BT n (tt <> tt') bts
           in   Just (bs', bt')

         | otherwise
         = let  (bss', bts')
                        = unzip
                        $ mapMaybe (collectForest (ns ++ [n]) (t <> t') (tt <> tt'))
                        $ zipWith Forest (unboxes gs) (unboxes bts)

                bs'     = concat bss'

                -- All the collected branches are guaranteed to have the
                -- same type because they were taken from the same path.
           in   case bts' of
                 []      -> Nothing
                 bt' : _ -> Just (bs', bt')

        -- Collect matching forests in a forest.
        collectForest ns t tt (Forest (G _ bs) bt)
         = let  (bss', bts')
                        = unzip
                        $ mapMaybe (collectTree ns t tt)
                        $ zipWith Tree   (unboxes bs) (repeat bt)

                bs'     = concat bss'

                -- All the collected branches are guaranteed to have the
                -- same type because they were taken from the same path.
           in   case bts' of
                 []      -> Nothing
                 bt' : _ -> Just (bs', bt')
