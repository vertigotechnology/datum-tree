
module Datum.Data.Tree.Operator.Elements
        ( takeElements
        , takeElement
        , takeAtoms
        , takeAtom
        , extractElement)
where
import Datum.Data.Tree.Operator.Strip
import Datum.Data.Tree.Operator.Extract
import Datum.Data.Tree.Operator.Project
import Datum.Data.Tree.Compounds
import Datum.Data.Tree.Exp
import Control.Monad



-- | Take the elements from an object.
takeElements :: HasKey obj => obj c -> [Element c]
takeElements o  = elementsOfKey (takeKey o)
{-# INLINE takeElements #-}


-- | Take the named element from an objects, if there is one.
takeElement  :: HasKey obj => Name -> obj c -> Maybe (Element c)
takeElement n o = elementOfKey n (takeKey o)
{-# INLINE takeElement #-}


-- | Extract the element with the given name from an object,
--   or `Nothing` if there isn't one. The expected type is set
--   by the calling context.
extractElement  :: (HasKey obj, Extractable (Element c) t)
                => Name -> obj c -> Maybe t
extractElement n o 
        = join 
        $ fmap extract
        $ elementOfKey n (takeKey o)


-- | Take the atoms from an object.
takeAtoms    :: HasKey obj => obj c -> [Atom]
takeAtoms o     = map strip $ elementsOfKey (takeKey o)
{-# INLINE takeAtoms #-}


-- | Take the atom of the given name from an object,
--   or `Nothing` if there isn't one.
takeAtom     :: HasKey obj => Name -> obj c -> Maybe Atom
takeAtom n o    = fmap strip $ elementOfKey n (takeKey o)
{-# INLINE takeAtom #-}



