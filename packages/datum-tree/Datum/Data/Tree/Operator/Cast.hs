
module Datum.Data.Tree.Operator.Cast
        ( weakenTree
        , weakenForest
        , promiseTree
        , promiseForest)
where
import Datum.Data.Tree.Exp


-- | Forget about the fact that we've checked this tree.
weakenTree   :: Tree c -> Tree 'X
weakenTree   (Tree b bt)    = Tree b bt
{-# INLINE weakenTree #-}


-- | Forget about the fact that we've checked this forest.
weakenForest :: Forest c -> Forest 'X
weakenForest (Forest bs bt) = Forest bs bt
{-# INLINE weakenForest #-}


-- | Promise that this tree is well typed.
promiseTree :: Tree c -> Tree c'
promiseTree (Tree b bt) = Tree b bt


-- | Promise that this forest is well typed.
promiseForest :: Forest c -> Forest c'
promiseForest (Forest bs bt) = Forest bs bt




