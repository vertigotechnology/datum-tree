
module Datum.Data.Tree.Check
        ( checkBranchType
        , checkTupleType

        , checkTree,    checkTree'
        , checkBranch

        , checkForest
        , checkGroup

        , checkKey,     checkKey'
        , checkTuple

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
-- | Check whether a branch type is well formed.
checkBranchType :: PathType -> BranchType -> Either Error ()
checkBranchType 
        (PathType pts)
        bt@(BT _n tKey subs)
 = do
        let pts'        = ITForest bt : pts
        let tPath'      = PathType pts'

        -- Check the tuple type.
        checkTupleType tPath' tKey

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | Box (BT n _ _) <- A.toList subs]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check the sub dimension shapes.
        mapM_ (checkBranchType tPath') 
                [b | Box b <- A.toList subs]


-- | Check that a tuple type is well formed.
checkTupleType :: PathType -> TupleType -> Either Error ()
checkTupleType path (TT nts)
 = do
        -- Check that field names do not clash.
        let nsField     = [n | Box n :*: _   <- A.toList nts]
        when (length nsField /= length (L.nub nsField))
         $ throwError $ ErrorClashField path nsField


-------------------------------------------------------------------------------
-- | Check whether a tree is well formed.
checkTree :: Tree c -> Either Error (Tree 'O)
checkTree tree 
 =      checkTree' mempty tree


-- | Check whether a tree is well formed, at the given starting path.
checkTree' :: Path -> Tree c -> Either Error (Tree 'O)
checkTree' path (Tree branch branchType)
 = case checkBranch path branch branchType of
        Left err -> Left err
        Right () -> Right $ Tree branch branchType


-- | Check whether a branch has the given branch type.
checkBranch :: Path -> Branch -> BranchType -> Either Error ()
checkBranch
        (Path ps pts) 
        (B key subs) bt@(BT name tKey@(TT _nts) tsSub)
 = do
        let ps'         = IForest name : ps
        let pts'        = ITForest bt  : pts
        let path'       = Path ps' pts'
        let tPath'      = PathType pts'

        -- Check the tuple type.
        checkTupleType tPath' tKey 

        -- Check the key matches its type.
        checkTuple   path'  key tKey

        -- Check that the number of sub trees matches the number of
        -- sub dimensions.
        when (length subs /= A.length tsSub)
         $ throwError $ ErrorArityDim path' subs tsSub

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | Box (BT n _ _) <- A.toList tsSub]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check each of the sub trees.
        zipWithM_ (checkGroup path') 
                subs 
                (map unbox $ A.toList tsSub)


-------------------------------------------------------------------------------
-- | Check whether a forest is well formed.
checkForest  :: Forest c -> Either Error (Forest 'O)
checkForest forest
        = checkForest' mempty forest


-- | Check whether a forest is well formed, at the given starting path.
checkForest' :: Path -> Forest c -> Either Error (Forest 'O)
checkForest' path (Forest bs bt)
 = case checkGroup path bs bt of
        Left err        -> Left err
        Right ()        -> Right $ Forest bs bt


-- | Check whether all the branches in a group
--   have the given branch type.
checkGroup :: Path -> Group -> BranchType -> Either Error ()
checkGroup path (G _n bs) shape
 = do   mapM_ (\b -> checkBranch path b shape) bs


-------------------------------------------------------------------------------
-- | Check whether a key is well formed.
checkKey  :: Key c -> Either Error (Key 'O)
checkKey key
        = checkKey' mempty key


-- | Check whether a key is well formed, with the given starting path.
checkKey' :: Path -> Key c -> Either Error (Key 'O)
checkKey' path (Key t tt)
 = case checkTuple path t tt of
        Left err        -> Left err
        Right ()        -> Right $ Key t tt


-- | Check whether a tuple has the given type.
checkTuple :: Path -> Tuple -> TupleType -> Either Error ()
checkTuple path@(Path _ps _pts) (T fields) (TT nts)
 = do   
        -- Check that the number of fields matches the tuple type.
        when (length fields /= A.length nts)
         $ throwError $ ErrorArityTuple path fields nts

        zipWithM_ 
                (\  field (_name :*: Box tField)
                 ->     checkAtom path field tField)
                fields 
                (A.toList nts)


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

