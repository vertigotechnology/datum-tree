
module Datum.Data.Tree.Compounds
        ( -- * Trees
          makeTree
        , takeTree
        , branchOfTree
        , typeOfTree
        , nameOfTree
        , forestsOfTree

          -- * Forests
        , makeForest
        , takeForest
        , groupOfForest
        , typeOfForest
        , nameOfForest
        , treesOfForest
        , forestOfTrees)
where
import Datum.Data.Tree.Exp


-- Trees ----------------------------------------------------------------------
-- | Make a tree from a branch and its branch type.
makeTree :: Branch -> BranchType -> Tree 'X
makeTree b bt = Tree b bt


-- | Take the branch and branch type from a tree.
takeTree :: Tree c -> (Branch, BranchType)
takeTree (Tree b bt) = (b, bt)


-- | Take the branch of a tree.
branchOfTree  :: Tree c -> Branch
branchOfTree (Tree b _) = b


-- | Take the branch type of a tree.
typeOfTree    :: Tree c -> BranchType
typeOfTree (Tree _ bt)  = bt


-- | Take the name of a tree from its branch type.
nameOfTree    :: Tree c -> Name
nameOfTree (Tree _ (BT n _ _)) = n


-- | Take the sub-forests of tree.
forestsOfTree :: Tree c -> [Forest c]
forestsOfTree (Tree (B _k gs) (BT _n _kt bts))
        = zipWith Forest gs bts


-- | Take the name of a forest from its branch type.
nameOfForest :: Forest c -> Name
nameOfForest (Forest _ (BT n _ _)) = n


-- | Take the sub-trees of a forest.
treesOfForest :: Forest c -> [Tree c]
treesOfForest (Forest (G _n bs) bt)
        = [Tree b bt | b <- bs]


-- Forests --------------------------------------------------------------------
-- | Make a forest from a branch group and its shared branchtype..
makeForest :: Group -> BranchType -> Forest 'X
makeForest b bt = Forest b bt


-- | Take a branch group and branch type from a forest.
takeForest :: Forest c  -> (Group, BranchType)
takeForest (Forest bs bt) = (bs, bt)


-- | Take the branch group from a forest.
groupOfForest :: Forest c -> Group
groupOfForest (Forest g _) = g


-- | Take the branch type from a forest.
typeOfForest :: Forest c -> BranchType
typeOfForest (Forest _ bt) = bt


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
        = Forest (G (Just n) (map branchOfTree trees)) bt



