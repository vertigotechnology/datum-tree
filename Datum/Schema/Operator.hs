{-# LANGUAGE ParallelListComp #-}
module Datum.Schema.Operator
        ( -- * Conversion
          makeForest,   takeForest
        , branchOfTree
        , treesOfForest

          -- * Mapping
        , mapBranches
        , mapForest

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
        , sliceBranches)
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


-- Forest Construction --------------------------------------------------------
-- | Make a forest from a group of branches and their common branchtype.
makeForest :: Group -> BranchType -> Forest
makeForest b bt = Forest b bt


-- | Take a group of branches and the branch type from a forest.
takeForest :: Forest   -> (Group, BranchType)
takeForest (Forest bs bt)
        = (bs, bt)


-- | Take the branch group from a forest.
groupOfForest :: Forest -> Group
groupOfForest (Forest g _)
        = g


-- | Take the branch type from a forest.
typeOfForest :: Forest -> BranchType
typeOfForest (Forest _ bt) = bt


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


-- Mapping --------------------------------------------------------------------
-- | Apply a per-tree function to every sub-tree of a tree.
mapBranches   :: (Path -> Tree -> Tree) -> Path -> Tree -> Tree
mapBranches f 
        (Path ps pts) 
        (Tree (B k0 xssSub0) (BT n0 kt0 tsSub0))
 = let
        path'   = Path (ISub k0 : ps) (ITSub n0 kt0 : pts)

        (xssSub1, tsSub1)
                = unzip
                $ map takeForest
                $ map (mapForest f path')
                $ zipWith makeForest xssSub0 tsSub0

   in   Tree (B k0 xssSub1) (BT n0 kt0 tsSub1)


-- | Apply a per-tree-function to a forest.
--
--   The result type of each per-tree function must be identifical.
--
mapForest :: (Path -> Tree -> Tree) -> Path -> Forest -> Forest
mapForest f path (Forest (G bs0) bt0)
 = let  
        trees           = map (\b -> Tree b bt0) bs0

        trees'          = [ f path tree      
                                | tree  <- trees
                                | i     <- [0..]]

        -- TODO: Check all the return types are identical at this point.
        (bs', bts')     = unzip $ map (\(Tree b bt) -> (b, bt)) trees'

   in   case bts' of
         []             -> Forest (G bs') bt0
         (bt': _)       -> Forest (G bs') bt'



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
                                = mapForest f path'
                                $ mapForest (traverseTree f) path'
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
filterTree p 
        (Path ps pts)
        (Tree (B k0 gs0) (BT n0 kt0 tsSub0))
 = let
        path'   = Path (ISub k0 : ps) (ITSub n0 kt0 : pts)

        ff      = map (mapForest (filterTree p) path')
                $ zipWith makeForest gs0 tsSub0

        (gs1, tsSub)
                = unzip
                $ [ ( G (map branchOfTree 
                                $ filter (p path') [Tree b tSub | b <- bsSub])
                    , tSub)
                  | Forest (G bsSub) tSub@(BT n _ _) <- ff ]

   in   Tree (B k0 gs1) (BT n0 kt0 tsSub)


-- | Filter a forest by keeping only the sub-trees that match
--   the given predicate.
filterForest :: (Path -> Tree -> Bool) -> Path -> Forest -> Forest
filterForest p path forest
        = forestOfTrees (typeOfForest forest)
        $ map    (filterTree p path)
        $ filter (p path)
        $ treesOfForest forest


-- | Keep only branches with the given names.
sliceBranches :: [Name] -> Tree -> Tree
sliceBranches ns (Tree (B k0 gs0) (BT n0 kt tsSub0))
 = let
        ff      = map (mapForest (\p t -> sliceBranches ns t) mempty)
                $ zipWith makeForest gs0 tsSub0

        (gs1, tsSub)
                = unzip
                $ [ (xsSub, tSub)
                        | Forest xsSub tSub@(BT n _ _) <- ff
                        , elem n ns]

   in   Tree (B k0 gs1) (BT n0 kt tsSub)

