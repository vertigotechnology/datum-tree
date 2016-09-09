
module Datum.Data.Tree.Operator.Count
        ( countAsFieldForest
        , countAsFieldTree)
where
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Cast
import Datum.Data.Tree.Exp
import Data.Maybe
import Data.Monoid
import qualified Data.Repa.Array        as A


-- | For each tree in a forest, count the number of branches in a given
--   subdimension and store the result as a new field.
countAsFieldForest
        :: Name         -- ^ Name of sub-dimension to count.
        -> Name         -- ^ Name of new field in tree.
        -> Forest 'O    
        -> Forest 'O

countAsFieldForest nField nDim forest
 = promiseForest
 $ mapTreesOfForest 
        (\_p tree -> countAsFieldTree nField nDim tree) 
        mempty forest  


-- | Count the number of branches in the given sub-dimension and store
--   the result as a new field.
countAsFieldTree
        :: Name         -- ^ Name of sub-dimension to count.
        -> Name         -- ^ Name of new field in tree.
        -> Tree 'O
        -> Tree 'O

countAsFieldTree nField nDim  
        tree@(Tree (B t0 gs0) (BT n0 tt0 bts0))
 = let
        sel (G _ bs) (BT n _ _)
         | n == nDim    = Just $ A.length bs
         | otherwise    = Nothing

   in   case catMaybes $ zipWith sel (unboxes gs0) (unboxes bts0) of
         [count]        
          -> Tree (B     (t0  <> (T  $ A.fromList [Box (ANat count)])) gs0)
                  (BT n0 (tt0 <> (TT $ A.fromList [Box nField :*: Box ATNat])) bts0)
 

         _ -> tree



