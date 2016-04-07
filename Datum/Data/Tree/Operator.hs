
module Datum.Data.Tree.Operator
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

          -- * Paths
        , enterTree
        , enterForest
        , pathIncludesName
        , onPath 

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

          -- * Slicing
        , sliceTree
        , sliceTreeWithNames)
where
import Datum.Data.Tree.Exp
import qualified Data.List              as L
import Debug.Trace


-- Tree Construction ------------------------------------------------------------------------------
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


-- Forest Construction ----------------------------------------------------------------------------
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


-- Paths ------------------------------------------------------------------------------------------
-- | Extend a path to record the fact that we have entered into this sub-tree.
enterTree :: Tree -> Path -> Path 
enterTree   (Tree (B k0 _) (BT n0 kt0 _)) (Path ps pts)
 = Path (ITree k0 : ps) (ITTree kt0 : pts)


-- | Extend a path to record the fact that we have entered into this sub-forest.
enterForest :: Forest -> Path -> Path
enterForest (Forest _ bt@(BT n0 _ bts)) (Path ps pts)
 = Path (IForest n0 : ps) (ITForest bt : pts)


-- | Check if a path includes a forest with the give name.
pathIncludesName :: Name -> Path -> Bool
pathIncludesName name path@(Path ps pts)
 = let  ns      = [n | IForest n <- ps]
   in   elem name ns


-- | Get the dimension names of a path.
dimNamesOfPath :: Path -> [Name]
dimNamesOfPath (Path ps pts)
 = [n | IForest n <- ps]


-- | Check if we're on the path defined by the given names.
onPath :: [Name] -> Path -> Bool
onPath ns (Path ps pts)
 = onPath' ns (Path (reverse ps) (reverse pts))

onPath' _  (Path [] _)  = True
onPath' [] _            = True
onPath' nn@(n : ns) pp@(Path (p : ps) (pt : pts))
 = case p of
        IForest n'
         | n == n'      -> onPath' ns (Path ps pts)
         | otherwise    -> False

        _               -> onPath' nn (Path ps pts)


-- Mapping ----------------------------------------------------------------------------------------
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
 = applyTreesOfForest
        (map (\tree -> f (enterTree tree path) tree))
        forest


-- | Apply a per-forest function to every sub-forest of a tree.
mapForestsOfTree :: (Path -> Forest -> Forest) -> Path -> Tree -> Tree
mapForestsOfTree f path tree
 = applyForestsOfTree 
        (map (\forest -> f (enterForest forest path) forest)) 
        tree


-- Application ------------------------------------------------------------------------------------
-- | Apply a function to the list of sub-trees of a forest.
-- 
--   * The worker function can change the type of the trees,
--     provided the result trees all have the same type.
--
applyTreesOfForest :: ([Tree] -> [Tree]) -> Forest -> Forest
applyTreesOfForest f forest
 = let  
        trees   = treesOfForest forest
        trees'  = f trees

   in   case trees' of
         []       -> forestOfTrees (typeOfForest forest) trees'
         (t0 : _) -> forestOfTrees (typeOfTree   t0)     trees'
 

-- | Apply a function to the list of sub-forests of a tree.
applyForestsOfTree :: ([Forest] -> [Forest]) -> Tree -> Tree
applyForestsOfTree f
        tree@(Tree (B k0 gs0) (BT n0 kt0 bts0))
 = let
        (gs1, bts1)
                = unzip
                $ map     takeForest
                $ f
                $ zipWith makeForest gs0 bts0

   in   Tree (B k0 gs1) (BT n0 kt0 bts1)


-- Pathless Mapping -------------------------------------------------------------------------------
-- | Like `mapTreesOfForest`, but we don't care about the path.
mapTreesOfForest'   :: (Tree -> Tree) -> Forest -> Forest
mapTreesOfForest' f forest
        = mapTreesOfForest   (\_path tree -> f tree) mempty forest   


-- Traversal --------------------------------------------------------------------------------------
-- | Bottom-up traversal of a tree.
traverseTree :: (Path -> Tree -> Tree) -> Path -> Tree -> Tree
traverseTree f  
        (Path ps pts)
        (Tree (B k0 xssSub0) (BT n0 kt0 tsSub0))
 = let
        path'   = Path (ITree k0 : ps) (ITTree kt0 : pts)

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


-- Reduction --------------------------------------------------------------------------------------
-- | Reduce all sub-trees in the given tree.
reduceTree :: (Path -> a -> Tree -> a) -> Path -> a -> Tree -> a
reduceTree f 
        (Path ps pts) 
        acc tree@(Tree (B k0 xssSub0) (BT n0 kt0 tsSub0))
 = let  
        path'   = Path (ITree k0 : ps) (ITTree kt0 : pts)
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
                                        | ITree  (T as) <- ps' ]))

                        (TT (reverse $ concat
                                [ reverse nts
                                        | ITTree (TT nts) <- pts']))

                  | Path ps' pts' <- paths]
  in    ixs


-- | Get the size of a tree, in number of branches, including the root branch.
sizeOfTree :: Tree -> Int
sizeOfTree tree
 = reduceTree (\_ n _ -> n + 1) mempty 0 tree   


-- Filter -----------------------------------------------------------------------------------------
-- | Filter a tree by keeping only the sub-trees that match
--   the given predicate. 
filterTree :: (Path -> Tree -> Bool) -> Path -> Tree -> Tree
filterTree pred path tree 
 = applyForestsOfTree 
        (\ forests 
        -> map  (\forest 
                -> filterTreesOfForest pred 
                        (enterForest forest path) 
                        forest)
                forests)
        tree


-- | Filter a tree by keeping only the sub-forests that match
--   the given predicate.
filterForestsOfTree :: (Path -> Forest -> Bool) -> Path -> Tree -> Tree
filterForestsOfTree pred path tree
 = applyForestsOfTree 
        (filter (\ forest 
                -> pred (enterForest forest path) forest))
        tree


-- | Filter a forest by keeping only the sub-trees that match
--   the given predicate.
--
--   * This is a shallow filter. We only consider the trees that directly
--     make up the given forest.
--
filterTreesOfForest :: (Path -> Tree -> Bool) -> Path -> Forest -> Forest
filterTreesOfForest pred path forest 
 = applyTreesOfForest
        (filter (\ tree
                -> pred (enterTree tree path) tree))
        forest


-- Slice ------------------------------------------------------------------------------------------
-- | Slice a tree by keeping only the sub-forests that satisfy the given
--   predicate.
sliceTree :: (Path -> Forest -> Bool) -> Path -> Tree -> Tree
sliceTree pred path0 tree0 
 = let path1    = enterTree tree0 path0
   in  applyForestsOfTree
        (\ forests
        -> map  (\ forest
                -> let path2    = enterForest forest path1
                   in  applyTreesOfForest 
                        (\ trees
                        -> map  (\ tree 
                                -> let path3   = enterTree tree path2
                                   in  sliceTree pred path3 tree) 
                                trees)
                        forest)
        $ filter
                (\ forest 
                -> let path1    = enterForest forest $ enterTree tree0 path0
                   in  pred path1 forest)
        $ forests)
        tree0


-- | Slice a tree by keeping only the sub-trees that have the 
--   given names.
--
--   * This is a deep filter. We consider the direct sub-trees of
--     the provided tree, as well as sub-trees of those, and so on.
--
sliceTreeWithNames :: [Name] -> Tree -> Tree
sliceTreeWithNames ns tree
 = sliceTree (\p _ -> onPath ns p) mempty tree


