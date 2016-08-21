
module Datum.Data.Tree.Operator.Forests
        ( HasForests (..)
        , mapForests
        , filterForests)
where
import Datum.Data.Tree.Operator.Map
import Datum.Data.Tree.Operator.Filter
import Datum.Data.Tree.Exp


class HasForests obj where
 -- | Take the sub-forests of an object.
 takeForests    :: obj c -> [Forest c]

 -- | Apply a function to every sub-forest of an object.
 mapForests'    :: (Path -> Forest c -> Forest c')
                ->  Path -> obj c    -> obj 'X

 -- | Apply a function to a single forest of an object.
 mapForest'     :: Name
                -> (Path -> Forest c -> Forest c')
                ->  Path -> obj c    -> obj 'X

 -- | Filter the sub-forests of an object.
 filterForests' :: (Path -> Forest c -> Bool)
                ->  Path -> obj c    -> obj c


instance HasForests Tree where
 takeForests    = forestsOfTree
 mapForests'    = mapForestsOfTree
 mapForest'     = mapForestOfTree
 filterForests' = filterForestsOfTree


-- | Apply a function to every sub-forests of an object.
mapForests :: HasForests obj => (Forest c -> Forest c') -> obj c -> obj 'X
mapForests f o     = mapForests'    (\_ t -> f t) mempty o
{-# INLINE mapForests #-}


-- | Filter the sub-forests of an object.
filterForests :: HasForests obj => (Forest c -> Bool) -> obj c -> obj c
filterForests f o  = filterForests' (\_ t -> f t) mempty o
{-# INLINE filterForests #-} 

