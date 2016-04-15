
module Datum.Data.Tree.Compounds
        ( -- * Special Trees
          emptyTree
        , emptyForest 

          -- * Trees
        , makeTree
        , takeTree
        , forestsOfTree
        , isLeaf
        , isLeafBranch

          -- * Forests
        , makeForest
        , takeForest
        , treesOfForest
        , forestOfTrees)
where
import Datum.Data.Tree.Exp
import Datum.Data.Tree.Operator.Project
import qualified Data.Repa.Array        as A


-- Special Trees --------------------------------------------------------------
-- | The empty tree.
emptyTree :: Tree c
emptyTree 
 = Tree   (B  (T A.empty) A.empty)
          (BT "root" mempty A.empty)


-- | The empty forest.
emptyForest :: Forest c
emptyForest
 = Forest (G  None A.empty)
          (BT "root" mempty A.empty)


-- Trees ----------------------------------------------------------------------
-- | Make a tree from a branch and its branch type.
makeTree :: Branch -> BranchType -> Tree 'X
makeTree b bt = Tree b bt


-- | Take the branch and branch type from a tree.
takeTree :: Tree c -> (Branch, BranchType)
takeTree (Tree b bt) = (b, bt)


-- | Take the sub-forests of tree.
forestsOfTree :: Tree c -> [Forest c]
forestsOfTree (Tree (B _k gs) (BT _n _kt bts))
 = let  Just fs = A.map2 (\(Box g) (Box bt) -> Box (Forest g bt))
                        gs bts
   in   [f | Box f <- A.toList fs]


-- | Check if a tree is a leaf, meaning it has no sub trees.
isLeaf :: Tree c -> Bool
isLeaf (Tree (B _ gs) _)
        = A.length gs == 0

isLeafBranch :: Branch -> Bool
isLeafBranch (B _ gs)
        = A.length gs == 0


-- Forests --------------------------------------------------------------------
-- | Make a forest from a branch group and its shared branchtype..
makeForest :: Group -> BranchType -> Forest 'X
makeForest b bt = Forest b bt


-- | Take a branch group and branch type from a forest.
takeForest :: Forest c  -> (Group, BranchType)
takeForest (Forest bs bt) = (bs, bt)


-- | Take the sub-trees of a forest.
treesOfForest :: Forest c -> [Tree c]
treesOfForest (Forest (G _n bs) bt)
        = [Tree b bt | Box b <- A.toList bs]


-- | Make a forest from a shared branch type and a list of trees.
--
--   The branch type must be supplied explicitly so we know what it should
--   be when the list of trees is empty.
-- 
--   * This function does not check that the provided trees all share the
--     same branch type, nor that the shared type is the same as the one
--     provided.
-- 
forestOfTrees :: BranchType -> [Tree c] -> Forest 'X
forestOfTrees bt@(BT n _ _) trees
        = Forest (G (Some n) 
                    (A.fromList $ map (box . takeData) trees)) 
                 bt

