
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


-- Tree Construction ------------------------------------------------------------------------------
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


-- | Take the sub forests of tree.
forestsOfTree :: Tree c -> [Forest c]
forestsOfTree (Tree (B _k gs) (BT _n _kt bts))
        = zipWith Forest gs bts


-- Forest Construction ----------------------------------------------------------------------------
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


-- | Take the name of a forest from its branch type.
nameOfForest :: Forest c -> Name
nameOfForest (Forest _ (BT n _ _)) = n


-- | Take the list of trees from a forest.
treesOfForest :: Forest c -> [Tree c]
treesOfForest (Forest (G _n bs) bt)
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
forestOfTrees :: BranchType -> [Tree c] -> Forest 'X
forestOfTrees bt@(BT n _ _) trees
        = Forest (G (Just n) (map branchOfTree trees)) bt


-- Paths ------------------------------------------------------------------------------------------
-- | Extend a path to record the fact that we have entered into this sub-tree.
enterTree :: Tree c -> Path -> Path 
enterTree   (Tree (B k0 _) (BT _n0 kt0 _)) (Path ps pts)
 = Path (ITree k0 : ps) (ITTree kt0 : pts)


-- | Extend a path to record the fact that we have entered into this sub-forest.
enterForest :: Forest c -> Path -> Path
enterForest (Forest _ bt@(BT n0 _ _bts)) (Path ps pts)
 = Path (IForest n0 : ps) (ITForest bt : pts)


-- | Check if a path includes a forest with the give name.
pathIncludesName :: Name -> Path -> Bool
pathIncludesName name (Path ps _pts)
 = let  ns      = [n | IForest n <- ps]
   in   elem name ns


-- | Check if we're on the path defined by the given names.
onPath :: [Name] -> Path -> Bool
onPath ns (Path ps pts)
 = onPath' ns (Path (reverse ps) (reverse pts))

onPath' _  (Path [] _)  = True
onPath' _  (Path _ [])  = True
onPath' [] _            = True
onPath' nn@(n : ns) (Path (p : ps) (_pt : pts))
 = case p of
        IForest n'
         | n == n'      -> onPath' ns (Path ps pts)
         | otherwise    -> False

        _               -> onPath' nn (Path ps pts)


-- Mapping ----------------------------------------------------------------------------------------
-- | Apply a per-tree function to every sub-tree of a tree.
mapTreesOfTree   :: (Path -> Tree c -> Tree c') -> Path -> Tree c -> Tree 'X
mapTreesOfTree f path tree
        = mapForestsOfTree (mapTreesOfForest f) path tree


-- | Apply a per-tree-function to every sub-tree of a forest.
mapTreesOfForest :: (Path -> Tree c -> Tree c') -> Path -> Forest c -> Forest 'X
mapTreesOfForest f path forest
 = applyTreesOfForest
        (map (\tree -> f (enterTree tree path) tree))
        forest


-- | Apply a per-forest function to every sub-forest of a tree.
mapForestsOfTree :: (Path -> Forest c -> Forest c') -> Path -> Tree c -> Tree 'X
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
applyTreesOfForest :: ([Tree c] -> [Tree c']) -> Forest c -> Forest 'X
applyTreesOfForest f forest
 = let  
        trees   = treesOfForest forest
        trees'  = f trees

   in   case trees' of
         []       -> forestOfTrees (typeOfForest forest) trees'
         (t0 : _) -> forestOfTrees (typeOfTree   t0)     trees'
 

-- | Apply a function to the list of sub-forests of a tree.
applyForestsOfTree :: ([Forest c] -> [Forest c']) -> Tree c -> Tree 'X
applyForestsOfTree f
        (Tree (B k0 gs0) (BT n0 kt0 bts0))
 = let
        (gs1, bts1)
                = unzip
                $ map     takeForest
                $ f
                $ zipWith Forest gs0 bts0

   in   Tree (B k0 gs1) (BT n0 kt0 bts1)


-- Traversal --------------------------------------------------------------------------------------
-- | Bottom-up traversal of a tree.
traverseTree :: (Path -> Tree 'X -> Tree c) -> Path -> Tree c' -> Tree 'X
traverseTree f  
        (Path ps pts)
        (Tree (B k0 xssSub0) (BT n0 kt0 tsSub0))
 = let
        path'   = Path (ITree k0 : ps) (ITTree kt0 : pts)

        (xssSub, tsSub)
                = unzip
                $ [ let Forest xsSub' tSub'
                                = mapTreesOfForest f path'
                                $ mapTreesOfForest (traverseTree f) path'
                                $ Forest xsSub tSub

                    in  (xsSub', tSub')

                        | xsSub <- xssSub0
                        | tSub  <- tsSub0 ]

   in   Tree (B k0 xssSub) (BT n0 kt0 tsSub)


-- Reduction --------------------------------------------------------------------------------------
-- | Reduce all sub-trees in the given tree.
reduceTree :: (Path -> a -> Tree c -> a) -> Path -> a -> Tree c -> a
reduceTree f 
        (Path ps pts) 
        acc tree@(Tree (B k0 _xssSub0) (BT _n0 kt0 _tsSub0))
 = let  
        path'   = Path (ITree k0 : ps) (ITTree kt0 : pts)

   in   L.foldl' (reduceForest f path') (f path' acc tree) 
         $ forestsOfTree tree


-- | Reduce all sub-trees in the given forest.
reduceForest :: (Path -> a -> Tree c -> a) -> Path -> a -> Forest c -> a
reduceForest f path acc (Forest (G _n bs) bt)
 =      L.foldl' (\acc' b -> reduceTree f path acc' (Tree b bt))
                 acc bs


-- | Get a list of all tuples in the given tree.
keysOfTree :: Tree c -> [Key c]
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
sizeOfTree :: Tree c -> Int
sizeOfTree tree
 = reduceTree (\_ n _ -> n + 1) mempty 0 tree   


-- Filter -----------------------------------------------------------------------------------------
-- | Filter a tree by keeping only the sub-trees that match
--   the given predicate. 
filterTree :: (Path -> Tree c -> Bool) -> Path -> Tree c -> Tree c
filterTree p path tree 
 = promiseTree
 $ applyForestsOfTree 
        (\ forests 
        -> map  (\forest 
                -> filterTreesOfForest p 
                        (enterForest forest path) 
                        forest)
                forests)
        tree


-- | Filter a tree by keeping only the sub-forests that match
--   the given predicate.
filterForestsOfTree :: (Path -> Forest c -> Bool) -> Path -> Tree c -> Tree c
filterForestsOfTree p path tree
 = promiseTree
 $ applyForestsOfTree 
        (filter (\ forest 
                -> p (enterForest forest path) forest))
        tree


-- | Filter a forest by keeping only the sub-trees that match
--   the given predicate.
--
--   * This is a shallow filter. We only consider the trees that directly
--     make up the given forest.
--
filterTreesOfForest :: (Path -> Tree c -> Bool) -> Path -> Forest c -> Forest c
filterTreesOfForest p path forest 
 = promiseForest
 $ applyTreesOfForest
        (filter (\ tree
                -> p (enterTree tree path) tree))
        forest


-- Slice ------------------------------------------------------------------------------------------
-- | Slice a tree by keeping only the sub-forests that satisfy the given
--   predicate.
sliceTree :: (Path -> Forest c -> Bool) -> Path -> Tree c -> Tree c
sliceTree p path0 tree0 
 = let path1    = enterTree tree0 path0
   in  promiseTree 
        $ applyForestsOfTree
                (\ forests
                -> map  (\ forest
                        -> let path2    = enterForest forest path1
                           in  applyTreesOfForest 
                                (\ trees
                                -> map  (\ tree 
                                        -> let path3   = enterTree tree path2
                                           in  sliceTree p path3 tree) 
                                        trees)
                                forest)
                $ filter
                        (\ forest 
                        -> let path2    = enterForest forest $ enterTree tree0 path0
                           in  p path2 forest)
                $ forests)
                tree0


-- | Slice a tree by keeping only the sub-trees that have the 
--   given names.
--
--   * This is a deep filter. We consider the direct sub-trees of
--     the provided tree, as well as sub-trees of those, and so on.
--
sliceTreeWithNames :: [Name] -> Tree c -> Tree c
sliceTreeWithNames ns tree
 = sliceTree (\p _ -> onPath ns p) mempty tree



promiseTree :: Tree c -> Tree c'
promiseTree (Tree b bt) = Tree b bt

promiseForest :: Forest c -> Forest c'
promiseForest (Forest bs bt) = Forest bs bt