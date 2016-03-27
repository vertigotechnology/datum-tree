{-# LANGUAGE ParallelListComp #-}
module Datum.Schema.Operator
        ( -- * Conversion
          toForest, fromForest

          -- * Mapping
        , mapBranches
        , mapForest

          -- * Traversal
        , traverseTree

          -- * Filtering
        , filterDim)
where
import Datum.Schema.Exp


-- Conversion -----------------------------------------------------------------
toForest   :: [Branch] -> BranchType    -> Forest
toForest b bt = Forest b bt


fromForest :: Forest   -> ([Branch], BranchType)
fromForest (Forest bs bt)
        = (bs, bt)


-- Mapping --------------------------------------------------------------------
-- | Apply a per-tree function to every sub-tree of a tree.
mapBranches   :: (Path -> Tree -> Tree) -> Path -> Tree -> Tree
mapBranches f path (Tree (B k0 xssSub0) (BT n0 kt tsSub0))
 = let
        (xssSub1, tsSub1)
                = unzip
                $ map fromForest
                $ map (mapForest f (IxSub n0 : path))
                $ zipWith toForest xssSub0 tsSub0

   in   Tree (B k0 xssSub1) (BT n0 kt tsSub1)


-- | Apply a per-tree-function to a forest.
--
--   The result type of each per-tree function must be identifical.
--
mapForest :: (Path -> Tree -> Tree) -> Path -> Forest -> Forest
mapForest f path (Forest bs0 bt0)
 = let  
        trees           = map (\b -> Tree b bt0) bs0

        trees'          = [ f (IxElem i : path) tree      
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
traverseTree f  path (Tree (B k0 xssSub0) (BT n0 kt0 tsSub0))
 = let
        path'   = IxSub n0 : path

        (xssSub, tsSub)
                = unzip
                $ [ let BT n kt _       = tSub
                        path''          = IxElem i : path'

                        Forest xsSub' tSub'
                                = mapForest f path'
                                $ mapForest (traverseTree f) path'
                                $ Forest xsSub tSub

                    in  (xsSub', tSub')

                        | xsSub <- xssSub0
                        | tSub  <- tsSub0
                        | i     <- [0..] ]

   in   Tree (B k0 xssSub) (BT n0 kt0 tsSub)


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

