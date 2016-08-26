
module Datum.Data.Tree.Operator.Append
        ( appendTrees
        , appendForests

        , concatTrees
        , concatForests)
where
import Datum.Data.Tree.Exp
import qualified Data.Repa.Array        as A
import qualified Data.List              as List

-------------------------------------------------------------------------------
-- | Concatenate data in the sub-dimensions of a list of trees.
concatTrees   :: [Tree c] -> Maybe (Tree c)

concatTrees [] 
 = Just emptyTree

concatTrees (t1: ts)
 | (b1, bt1)    <- takeTree t1
 , (bs, bts)    <- unzip $ map takeTree ts
 , all (== bt1) bts
 , Just b'      <- concatBranches (b1 : bs)
 = Just $ Tree b' bt1

 | otherwise
 = Nothing


-- | Concatenate data in the sub-dimenions of a list of forests.
concatForests  :: [Forest c] -> Maybe (Forest c)

concatForests []
 = Just emptyForest

concatForests (f1 : fs)
 | (g1, bt1)    <- takeForest f1
 , (gs, bts)    <- unzip $ map takeForest fs
 , all (== bt1) bts
 , Just g'      <- concatGroups (g1 : gs)
 = Just $ Forest g' bt1

 | otherwise
 = Nothing


-- | Concatenate data in the subdimensions of several branches.
concatBranches :: [Branch] -> Maybe Branch

concatBranches []       
 = Just (B mempty A.empty)

concatBranches bs       
 | (ts, gss)    <- unzip [(t, gs) | B t gs    <- bs]
 , tLast        <- last ts
 , Just gs'     <- sequence 
                        $ map concatGroups 
                        $ List.transpose 
                        $ map unboxes gss
 = Just $ B tLast (boxes gs')

 | otherwise
 = Nothing
 

-- | Concat data in several branch groups.
concatGroups  :: [Group] -> Maybe Group
concatGroups []
 = Just $ G None A.empty

concatGroups gs
 | (ons, bss)   <- unzip [ (on, bs) | G on bs <- gs]
 , Just on'     <- case List.nub [ n | Some n <- ons ] of
                        []      -> Just None
                        [n]     -> Just (Some n)
                        _       -> Nothing
 = let  !bs'    = concat $ map unboxes bss
   in   Just $ G on' (boxes bs')

 | otherwise
 = Nothing


-------------------------------------------------------------------------------
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
