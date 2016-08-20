
module Datum.Data.Tree.Operator.Reduce
        ( reduceTree
        , reduceForest
        , keysOfTree
        , sizeOfTree)
where
import Datum.Data.Tree.Compounds
import Datum.Data.Tree.Exp
import qualified Data.List              as L
import qualified Data.Repa.Array        as A


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
                 acc 
                 (map unbox $ A.toList bs)


-- | Get a list of all tuples in the given tree.
keysOfTree :: Tree c -> [Key c]
keysOfTree tree
 = let  
        paths   = reduceTree (\p acc _ -> acc ++ [p]) mempty [] tree
        ixs     = [ Key (T      $ A.fromList
                                $ reverse $ concat 
                                [ reverse (A.toList as )
                                        | ITree  (T as) <- ps' ])

                        (TT     $ A.fromList 
                                $ reverse $ concat
                                [ reverse $ A.toList nts
                                        | ITTree (TT nts) <- pts'])

                  | Path ps' pts' <- paths]
  in    ixs


-- | Get the size of a tree, in number of branches, including the root branch.
sizeOfTree :: Tree c -> Int
sizeOfTree tree
 = reduceTree (\_ n _ -> n + 1) mempty 0 tree   


