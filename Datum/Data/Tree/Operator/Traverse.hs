

module Datum.Data.Tree.Operator.Traverse
        (traverseTree)
where
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Exp
import qualified Data.Repa.Array        as A


-- | Bottom-up traversal of a tree.
--
--   * This does not check that the transformed trees are well
--     formed during traversal.
--
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

                        | xsSub  <- unboxes xssSub0
                        | tSub   <- unboxes tsSub0 ]

   in   Tree    (B k0      $ boxes xssSub) 
                (BT n0 kt0 $ boxes tsSub)

