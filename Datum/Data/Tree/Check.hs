
module Datum.Data.Tree.Check
        ( Check (..)

        , checkAtom

        , Error (..)
        , ppError)
where
import Datum.Data.Tree.Check.Error
import Datum.Data.Tree.Exp
import Control.Monad
import Control.Monad.Except
import qualified Data.List              as L
import qualified Data.Repa.Array        as A


-------------------------------------------------------------------------------
class Monoid (Path' a) => Check a where
 type Checked' a
 type Path'    a

 -- | Type check an object.
 check          ::            a -> Either Error (Checked' a)
 check = checkOnPath mempty

 -- | Type check an object that is on the given path in the tree.
 --   The path is used for error reporting only.
 checkOnPath    :: Path' a -> a -> Either Error (Checked' a)


-------------------------------------------------------------------------------
instance Check BranchType where
 type Checked' BranchType = BranchType
 type Path'    BranchType = PathType

 checkOnPath  (PathType pts) bt@(BT _n tKey subs)
  = do
        let pts'        = ITForest bt : pts
        let tPath'      = PathType pts'

        -- Check the tuple type.
        checkOnPath tPath' tKey

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | Box (BT n _ _) <- A.toList subs]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check the sub dimension shapes.
        mapM_ (checkOnPath tPath') 
                [b | Box b <- A.toList subs]

        return bt


instance Check TupleType where
 type Checked' TupleType = TupleType
 type Path'    TupleType = PathType

 checkOnPath path tt@(TT nts)
  = do
        -- Check that field names do not clash.
        let nsField     = [n | Box n :*: _   <- A.toList nts]
        when (length nsField /= length (L.nub nsField))
         $ throwError $ ErrorClashField path nsField

        return tt


-------------------------------------------------------------------------------
instance Check (Tree c) where
 type Checked' (Tree c) = Tree 'O
 type Path'    (Tree c) = Path

 checkOnPath 
        (Path ps pts) 
        (Tree b@(B t subs) bt@(BT name tt@(TT _nts) tsSub))
  = do
        let ps'         = IForest name : ps
        let pts'        = ITForest bt  : pts
        let path'       = Path ps' pts'
        let tPath'      = PathType pts'

        -- Check the tuple type.
        checkOnPath tPath' tt 

        -- Check the key matches its type.
        checkOnPath path'  (Key t tt)

        -- Check that the number of sub trees matches the number of
        -- sub dimensions.
        when (A.length subs /= A.length tsSub)
         $ throwError $ ErrorArityDim path' subs tsSub

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | Box (BT n _ _) <- A.toList tsSub]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check each of the sub forests.
        mapM_   (checkOnPath path') 
                $ zipWith Forest (unboxes subs) (unboxes tsSub)

        return (Tree b bt)


-------------------------------------------------------------------------------
instance Check (Forest c) where
 type Checked' (Forest c) = Forest 'O
 type Path'    (Forest c) = Path

 checkOnPath path (Forest g@(G _n bs) bt)
  = do  mapM_   (\b -> checkOnPath path (Tree b bt))
                [b | Box b <- A.toList bs]

        return  (Forest g bt)


-------------------------------------------------------------------------------
instance Check (Key c) where
 type Checked' (Key c) = Key 'O
 type Path'    (Key c) = Path

 checkOnPath 
        path@(Path _ps _pts) 
        (Key t@(T fields) tt@(TT nts))
  = do   
        -- Check that the number of fields matches the tuple type.
        when (A.length fields /= A.length nts)
         $ throwError $ ErrorArityTuple path fields nts

        zipWithM_
                (\  field (_name :*: Box tField)
                 ->     checkAtom path field tField)
                (unboxes fields)
                (A.toList nts)

        return (Key t tt)


-------------------------------------------------------------------------------
-- | Check that an atom has the given type.
checkAtom  :: Path -> Atom -> AtomType -> Either Error ()
checkAtom path lit tp
 = case (lit, tp) of
        (AUnit,         ATUnit)         -> return ()
        (ABool _,       ATBool)         -> return ()
        (AInt _,        ATInt)          -> return ()
        (AFloat _,      ATFloat)        -> return ()
        (ANat _,        ATNat)          -> return ()
        (ADecimal _,    ATDecimal)      -> return ()
        (AText _,       ATText)         -> return ()
        (ATime _,       ATTime)         -> return ()
        _ -> throwError $ ErrorAtom path lit tp

