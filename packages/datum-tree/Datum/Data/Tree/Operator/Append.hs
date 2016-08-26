
module Datum.Data.Tree.Operator.Append
        ( appendTrees
        , appendForests)
where
import Datum.Data.Tree.Exp
import qualified Data.Repa.Array        as A


-- | Append the data in the sub dimensions of two trees.
--
--   The root key for the result is taken from the second argument tree.
--
appendTrees   :: Tree c -> Tree c -> Maybe (Tree c)
appendTrees (Tree (B _t1 gs1) bt1) (Tree (B t2 gs2) bt2)
 | bt1 == bt2
 , Just gs'     <- sequence 
                $  zipWith appendGroups (unboxes gs1) (unboxes gs2)
 = Just $ Tree (B t2 (boxes gs')) bt2

 | otherwise
 = Nothing


-- | Append two forests.
--
--   They must have the type, otherwise `Nothing`.
appendForests :: Forest c -> Forest c -> Maybe (Forest c)
appendForests (Forest g1 bt1) (Forest g2 bt2)
 | bt1 == bt2
 , Just g'      <- appendGroups g1 g2
 = return $ Forest g' bt1

 | otherwise
 = Nothing



-- | Append two groups.
appendGroups :: Group -> Group -> Maybe Group
appendGroups (G g1 bs1) (G g2 bs2)
 | Just mg   
    <- case (g1, g2) of
        (None,    None)         -> Just  None
        (None,    Some n)       -> Just (Some n)
        (Some n,  None)         -> Just (Some n)
        (Some n1, Some n2)
          | n1 == n2            -> Just (Some n1)
          | otherwise           -> Nothing

 = return $ G mg $ A.fromList (A.toList bs1 ++ A.toList bs2)

 | otherwise
 = Nothing
