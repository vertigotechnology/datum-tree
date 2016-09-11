
module Datum.Data.Tree.Operator.Reduce
        ( reduceTree
        , reduceForest
        , keysOfTree
        , sizeOfTree
        , foldAsFieldTree
        , foldAsFieldForest)
where
import Datum.Data.Tree.Exp
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Cast
import Data.Monoid
import Data.Maybe
import qualified Data.Repa.Array        as A
import qualified Data.List              as List


-- | Reduce all sub-trees in the given tree.
reduceTree :: (Path -> a -> Tree c -> a) -> Path -> a -> Tree c -> a
reduceTree f 
        (Path ps pts) 
        acc tree@(Tree (B k0 _xssSub0) (BT _n0 kt0 _tsSub0))
 = let  
        path'   = Path (ITree k0 : ps) (ITTree kt0 : pts)

   in   List.foldl' (reduceForest f path') (f path' acc tree) 
         $ forestsOfTree tree


-- | Reduce all sub-trees in the given forest.
reduceForest :: (Path -> a -> Tree c -> a) -> Path -> a -> Forest c -> a
reduceForest f path acc (Forest (G _n bs) bt)
 =      List.foldl' (\acc' b -> reduceTree f path acc' (Tree b bt))
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



---------------------------------------------------------------------------------------------------
foldAsFieldForest
        :: Name
        -> Name
        -> (Path -> Atom -> Tree c   -> Atom)
        ->  Path -> Atom -> Forest c -> Forest c

foldAsFieldForest nField nDim fWork path0 aZero forest
 = promiseForest
 $ mapTreesOfForest
        (\p tree -> foldAsFieldTree nField nDim fWork p aZero tree)
        path0 forest


foldAsFieldTree
        :: Name                                 -- ^ Name of new field in key.
        -> Name                                 -- ^ Name of sub-dimension to fold over.
        -> (Path -> Atom -> Tree c -> Atom)
        ->  Path -> Atom -> Tree c -> Tree c

foldAsFieldTree 
        nField nDim fWork path0 aZero 
        tree0@(Tree (B t0 gs0) (BT n0 tt0 bts0))
 = let
        sel (G _ bs) bt@(BT n _ _)
         | n == nDim    = Just $ List.foldl' (fWork path0) aZero 
                               $ [Tree b bt | b <- unboxes bs]

         | otherwise    = Nothing

   in   case catMaybes $ zipWith sel (unboxes gs0) (unboxes bts0) of
         [aResult]        
          -> Tree (B     (t0  <> (T  $ A.fromList [Box aResult])) gs0)
                  (BT n0 (tt0 <> (TT $ A.fromList [Box nField :*: Box ATNat])) bts0)
 
         _ -> tree0


