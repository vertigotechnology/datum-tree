
module Datum.Data.Tree.Operator.Path
        ( enterTree
        , enterForest
        , pathIncludesName
        , onPath)
where
import Datum.Data.Tree.Exp


-- | Extend a path to record that we have entered into this sub-tree.
enterTree :: Tree c -> Path -> Path 
enterTree   (Tree (B k0 _) (BT _n0 kt0 _)) (Path ps pts)
 = Path (ITree k0 : ps) (ITTree kt0 : pts)


-- | Extend a path to record that we have entered into this sub-forest.
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

