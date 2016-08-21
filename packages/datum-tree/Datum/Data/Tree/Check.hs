
module Datum.Data.Tree.Check
        ( Check (..)
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
 check  ::            a -> Either Error (Checked' a)
 check = check' mempty

 -- | Type check an object that is on the given path in the tree.
 --   The path is used for error reporting only.
 check' :: Path' a -> a -> Either Error (Checked' a)


-------------------------------------------------------------------------------
instance Check BranchType where
 type Checked' BranchType = BranchType
 type Path'    BranchType = PathType

 check'  (PathType pts) bt@(BT _n tt subs)
  = do
        let pts'        = ITForest bt : pts
        let tPath'      = PathType pts'

        -- Check the tuple type.
        _ <- check' tPath' tt

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | Box (BT n _ _) <- A.toList subs]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check the sub dimension shapes.
        mapM_ (check' tPath') 
                [b | Box b <- A.toList subs]

        return bt


instance Check TupleType where
 type Checked' TupleType = TupleType
 type Path'    TupleType = PathType

 check' path tt@(TT nts)
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

 check' (Path ps pts) 
        (Tree b@(B t subs) bt@(BT name tt@(TT _nts) tsSub))
  = do
        -- Build the current path into the tree.
        let ps'         = IForest name : ps
        let pts'        = ITForest bt  : pts
        let path'       = Path ps' pts'
        let tPath'      = PathType pts'

        -- Check the tuple type.
        _ <- check' tPath' tt 

        -- Check the key matches its type.
        _ <- check' path'  (Key t tt)

        -- Check that if we have sub trees then the number of 
        -- sub trees matches the expected number of dimensions.
        -- It's ok not to have any sub trees at all.
        when (  (A.length subs /= 0)
             && (A.length subs /= A.length tsSub))
         $ throwError $ ErrorArityDim path' t subs tsSub

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | Box (BT n _ _) <- A.toList tsSub]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check each of the sub forests.
        mapM_   (check' path') 
         $ zipWith Forest (unboxes subs) (unboxes tsSub)

        return (Tree b bt)


-------------------------------------------------------------------------------
instance Check (Forest c) where
 type Checked' (Forest c) = Forest 'O
 type Path'    (Forest c) = Path

 check' path (Forest g@(G _n bs) bt)
  = do  mapM_   (\b -> check' path (Tree b bt))
                [b | Box b <- A.toList bs]

        return  (Forest g bt)


-------------------------------------------------------------------------------
instance Check (Key c) where
 type Checked' (Key c) = Key 'O
 type Path'    (Key c) = Path

 check' path@(Path _ps _pts) 
        (Key t@(T fields) tt@(TT nts))
  = do   
        -- Check that the number of fields matches the tuple type.
        when (A.length fields /= A.length nts)
         $ throwError $ ErrorArityTuple path fields nts

        zipWithM_
                (\  field (_name :*: Box tField)
                 ->     check' path (Element field tField))
                (unboxes fields)
                (A.toList nts)

        return (Key t tt)


-------------------------------------------------------------------------------
instance Check (Element c) where
 type Checked' (Element c) = Element 'O
 type Path'    (Element c) = Path

 check' path (Element a at)
  = case (a, at) of
        (AUnit,         ATUnit)     -> return (Element a at)
        (ABool _,       ATBool)     -> return (Element a at)
        (AInt _,        ATInt)      -> return (Element a at)
        (AFloat _,      ATFloat)    -> return (Element a at)
        (ANat _,        ATNat)      -> return (Element a at)
        (ADecimal _,    ATDecimal)  -> return (Element a at)
        (AText _,       ATText)     -> return (Element a at)
        (ATime _,       ATTime)     -> return (Element a at)
        _ -> throwError $ ErrorAtom path a at

