{-# LANGUAGE ParallelListComp #-}
module Datum.Schema.Operator
        ( -- * Conversion
          toForest, fromForest

          -- * Mapping
        , mapBranches
        , mapForest

          -- * Reducing
        , reduceTree
        , reduceForest

          -- * Special Reductions
        , keys

          -- * Traversal
        , traverseTree

          -- * Filtering
        , filterDim)
where
import Datum.Schema.Exp
import qualified Data.List              as L


-- Conversion -----------------------------------------------------------------
toForest   :: [Branch] -> BranchType    -> Forest
toForest b bt = Forest b bt


fromForest :: Forest   -> ([Branch], BranchType)
fromForest (Forest bs bt)
        = (bs, bt)


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
                $ map fromForest
                $ map (mapForest f path')
                $ zipWith toForest xssSub0 tsSub0

   in   Tree (B k0 xssSub1) (BT n0 kt0 tsSub1)


-- | Apply a per-tree-function to a forest.
--
--   The result type of each per-tree function must be identifical.
--
mapForest :: (Path -> Tree -> Tree) -> Path -> Forest -> Forest
mapForest f path (Forest bs0 bt0)
 = let  
        trees           = map (\b -> Tree b bt0) bs0

        trees'          = [ f path tree      
                                | tree  <- trees
                                | i     <- [0..]]

        -- TODO: Check all the return types are identical at this point.
        (bs', bts')     = unzip $ map (\(Tree b bt) -> (b, bt)) trees'

   in   case bts' of
         []             -> Forest bs' bt0
         (bt': _)       -> Forest bs' bt'



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
        forests = zipWith toForest xssSub0 tsSub0

   in   L.foldl' (reduceForest f path') (f path' acc tree) forests


-- | Reduce all sub-trees in the given forest.
reduceForest :: (Path -> a -> Tree -> a) -> Path -> a -> Forest -> a
reduceForest f path acc (Forest bs bt)
 =      L.foldl' (\acc' b -> reduceTree f path acc' (Tree b bt))
                 acc bs


-- | Get a list of all tuples in the given tree.
keys :: Tree -> [Key]
keys tree
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




-- Filtering ------------------------------------------------------------------
-- | Keep only the sub dimensions of the given names.
filterDim :: [Name] -> Tree -> Tree
filterDim ns (Tree (B k0 xssSub0) (BT n0 kt tsSub0))
 = let
        ff      = zipWith toForest xssSub0 tsSub0

        (xssSub, tsSub)
                = unzip
                $ [ (xsSub, tSub)
                        | Forest xsSub tSub@(BT n _ _) <- ff
                        , elem n ns]

   in   Tree (B k0 xssSub) (BT n0 kt tsSub)

