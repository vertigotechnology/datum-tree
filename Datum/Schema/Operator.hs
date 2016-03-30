{-# LANGUAGE ParallelListComp #-}
module Datum.Schema.Operator
        ( -- * Tree Construction
          makeTree,     takeTree
        , branchOfTree, typeOfTree
        , nameOfTree
        , forestsOfTree

          -- * Forest Construction
        , makeForest,   takeForest
        , groupOfForest, typeOfForest
        , nameOfForest
        , treesOfForest

          -- * Mapping
        , mapTreesOfTree
        , mapTreesOfForest
        , mapForestsOfTree

          -- * Reducing
        , reduceTree
        , reduceForest

          -- * Special Reductions
        , keysOfTree
        , sizeOfTree

          -- * Traversal
        , traverseTree

          -- * Filtering
        , filterTree
        , filterTreesOfForest
        , filterForestsOfTree
        , sliceTreeWithNames)
where
import Datum.Schema.Exp
import qualified Data.List              as L


-- Tree Construction ----------------------------------------------------------
-- | Make a tree from a branch and its branch type.
makeTree :: Branch -> BranchType -> Tree
makeTree b bt = Tree b bt


-- | Take the branch and branch type from a tree.
takeTree :: Tree -> (Branch, BranchType)
takeTree (Tree b bt) = (b, bt)


-- | Take the branch of a tree.
branchOfTree  :: Tree -> Branch
branchOfTree (Tree b _) = b


-- | Take the branch type of a tree.
typeOfTree    :: Tree -> BranchType
typeOfTree (Tree _ bt)  = bt


-- | Take the name of a tree.
nameOfTree    :: Tree -> Name
nameOfTree (Tree _ (BT n _ _)) = n


-- | Take the list of sub-forests from a tree.
forestsOfTree :: Tree -> [Forest]
forestsOfTree (Tree (B k gs) (BT n kt bts))
        = zipWith makeForest gs bts


-- Forest Construction --------------------------------------------------------
-- | Make a forest from a group of branches and their common branchtype.
makeForest :: Group -> BranchType -> Forest
makeForest b bt = Forest b bt


-- | Take a group of branches and the branch type from a forest.
takeForest :: Forest   -> (Group, BranchType)
takeForest (Forest bs bt) = (bs, bt)


-- | Take the branch group from a forest.
groupOfForest :: Forest -> Group
groupOfForest (Forest g _) = g


-- | Take the branch type from a forest.
typeOfForest :: Forest -> BranchType
typeOfForest (Forest _ bt) = bt


-- | Take the common branch name of a forest.
nameOfForest :: Forest -> Name
nameOfForest (Forest _ (BT n _ _)) = n


-- | Take a list of trees from a forest.
treesOfForest :: Forest -> [Tree]
treesOfForest (Forest (G bs) bt)
        = [Tree b bt | b <- bs]


-- | Make a forest from a shared branch type and a list of trees.
--
--   The branch type must be supplied explicitly so we know what it should
--   be when the list of trees is empty.
-- 
--   * This function does not check that the provided trees all share the
--     same branch type, nor that the shared type is the same as the one
--     provided.
-- 
forestOfTrees :: BranchType -> [Tree] -> Forest
forestOfTrees bt trees
        = Forest (G (map branchOfTree trees)) bt


-- Paths ----------------------------------------------------------------------
-- | Extend a path to record the fact that we have entered into this sub tree.
enterTree :: Tree -> Path -> Path 
enterTree (Tree (B k0 _) (BT n0 kt0 _)) (Path ps pts)
 = Path (ISub k0 : ps) (ITSub n0 kt0 : pts)


-- Mapping --------------------------------------------------------------------
-- | Apply a per-tree function to every sub-tree of a tree.
mapTreesOfTree   :: (Path -> Tree -> Tree) -> Path -> Tree -> Tree
mapTreesOfTree f path tree
        = mapForestsOfTree (mapTreesOfForest f) path tree


-- | Apply a per-tree-function to every sub-tree of a forest.
--
--   * The result type of each per-tree function must be identifical.
--
mapTreesOfForest :: (Path -> Tree -> Tree) -> Path -> Forest -> Forest
mapTreesOfForest f path forest
        = applyTreesOfForest (\path' -> map (f path')) path forest


-- | Apply a per-forest function to every sub-forest of a tree.
mapForestsOfTree :: (Path -> Forest -> Forest) -> Path -> Tree -> Tree
mapForestsOfTree f path tree
        = applyForestsOfTree (\path' -> map (f path')) path tree


-- | Apply a function to the list of sub-trees of a forest.
-- 
--   * The worker function can change the type of the trees,
--     provided the result trees all have the same type.
--
applyTreesOfForest :: (Path -> [Tree] -> [Tree]) -> Path -> Forest -> Forest
applyTreesOfForest f path forest
 = let  
        trees   = treesOfForest forest
        trees'  = f path trees

   in   case trees' of
         []       -> forestOfTrees (typeOfForest forest) trees'
         (t0 : _) -> forestOfTrees (typeOfTree   t0)     trees'
 

-- | Apply a function to the list of sub-forests of a tree.
applyForestsOfTree :: (Path -> [Forest] -> [Forest]) -> Path -> Tree -> Tree
applyForestsOfTree f path 
        tree@(Tree (B k0 gs0) (BT n0 kt0 bts0))
 = let
        path'   = enterTree tree path

        (gs1, bts1)
                = unzip
                $ map     takeForest
                $ f path'
                $ zipWith makeForest gs0 bts0

   in   Tree (B k0 gs1) (BT n0 kt0 bts1)


-- Pathless Mapping -----------------------------------------------------------
-- | Like `mapTreesOfForest`, but we don't care about the path.
mapTreesOfForest'   :: (Tree -> Tree) -> Forest -> Forest
mapTreesOfForest' f forest
        = mapTreesOfForest   (\_path tree -> f tree) mempty forest   


-- | Like `applyTreesOfForest`, but we don't care about the path.
applyTreesOfForest' :: ([Tree] -> [Tree]) -> Forest -> Forest
applyTreesOfForest' f forest
        = applyTreesOfForest (\_path trees -> f trees) mempty forest 


-- | Like `applyForestsOfTree`, but we don't care about the path.
applyForestsOfTree' :: ([Forest] -> [Forest]) -> Tree -> Tree
applyForestsOfTree' f tree
        = applyForestsOfTree (\_path forests -> f forests) mempty tree    


-- Traversal ------------------------------------------------------------------
-- | Bottom-up traversal of a tree.
traverseTree :: (Path -> Tree -> Tree) -> Path -> Tree -> Tree
traverseTree f  
        (Path ps pts)
        (Tree (B k0 xssSub0) (BT n0 kt0 tsSub0))
 = let
        path'   = Path (ISub k0 : ps) (ITSub n0 kt0 : pts)

        (xssSub, tsSub)
                = unzip
                $ [ let BT n kt _ = tSub

                        Forest xsSub' tSub'
                                = mapTreesOfForest f path'
                                $ mapTreesOfForest (traverseTree f) path'
                                $ Forest xsSub tSub

                    in  (xsSub', tSub')

                        | xsSub <- xssSub0
                        | tSub  <- tsSub0
                        | i     <- [0..] ]

   in   Tree (B k0 xssSub) (BT n0 kt0 tsSub)


-- Reduction ------------------------------------------------------------------
-- | Reduce all sub-trees in the given tree.
reduceTree :: (Path -> a -> Tree -> a) -> Path -> a -> Tree -> a
reduceTree f 
        (Path ps pts) 
        acc tree@(Tree (B k0 xssSub0) (BT n0 kt0 tsSub0))
 = let  
        path'   = Path (ISub k0 : ps) (ITSub n0 kt0 : pts)
        forests = zipWith makeForest xssSub0 tsSub0

   in   L.foldl' (reduceForest f path') (f path' acc tree) forests


-- | Reduce all sub-trees in the given forest.
reduceForest :: (Path -> a -> Tree -> a) -> Path -> a -> Forest -> a
reduceForest f path acc (Forest (G bs) bt)
 =      L.foldl' (\acc' b -> reduceTree f path acc' (Tree b bt))
                 acc bs


-- | Get a list of all tuples in the given tree.
keysOfTree :: Tree -> [Key]
keysOfTree tree
 = let  
        paths   = reduceTree (\p acc _ -> acc ++ [p]) mempty [] tree
        ixs     = [ Key (T  (reverse $ concat 
                                [ reverse as 
                                        | ISub  (T as) <- ps' ]))

                        (TT (reverse $ concat
                                [ reverse nts
                                        | ITSub _ (TT nts) <- pts']))

                  | Path ps' pts' <- paths]
  in    ixs


-- | Get the size of a tree, in number of branches, including the root branch.
sizeOfTree :: Tree -> Int
sizeOfTree tree
 = reduceTree (\_ n _ -> n + 1) mempty 0 tree   


-- Filtering ------------------------------------------------------------------
-- | Filter a tree by keeping only the sub-trees that match
--   the given predicate. 
filterTree :: (Path -> Tree -> Bool) -> Path -> Tree -> Tree
filterTree pred path tree
 = applyForestsOfTree 
        (\  path1 forests -> map (filterTreesOfForest pred path1) forests)
        path tree


-- | Filter a tree by keeping only the sub-forests that match
--   the given predicate.
filterForestsOfTree :: (Path -> Forest -> Bool) -> Path -> Tree -> Tree
filterForestsOfTree pred path tree
 = applyForestsOfTree  (\path1 -> filter (pred path1)) path tree


-- | Filter a forest by keeping only the sub-trees that match
--   the given predicate.
--
--   * This is a shallow filter. We only consider the trees that directly
--     make up the given forest.
--
filterTreesOfForest :: (Path -> Tree -> Bool) -> Path -> Forest -> Forest
filterTreesOfForest pred path forest
 = applyTreesOfForest 
        (\path' trees -> filter (pred path') trees)
        path forest


-- | Slice a tree by keeping only the sub-trees that have the 
--   given names.
--
--   * This is a deep filter. We consider the direct sub-trees of
--     the provided tree, as well as sub-trees of those, and so on.
--
sliceTreeWithNames :: [Name] -> Tree -> Tree
sliceTreeWithNames ns tree
 = applyForestsOfTree'
        ( filter (\forest -> elem (nameOfForest forest) ns)
        . map    (mapTreesOfForest' (sliceTreeWithNames ns)))
        tree


