
module Datum.Data.Tree.Check
        ( checkBranchType
        , checkKeyType

        , checkTree,    checkTree'
        , checkBranch

        , checkForest
        , checkGroup

        , checkKey,     checkKey'
        , checkTuple

        , checkAtom

        , Error (..))
where
import Datum.Data.Tree.Exp
import Control.Monad
import Control.Monad.Except
import qualified Data.List              as L


-------------------------------------------------------------------------------
-- | Check that a shape is well formed.
checkBranchType :: PathType -> BranchType -> Either Error ()
checkBranchType 
        (PathType pts)
        bt@(BT name tKey subs)
 = do
        let pts'        = ITForest bt : pts
        let tPath'      = PathType pts'

        -- Check the tuple type.
        checkKeyType tPath' tKey

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | BT n _ _ <- subs]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check the sub dimension shapes.
        mapM_ (checkBranchType tPath') subs


-- | Check that a tuple type is well formed.
checkKeyType :: PathType -> TupleType -> Either Error ()
checkKeyType path (TT nts)
 = do
        -- Check that field names do not clash.
        let nsField     = [n | (n, _)   <- nts]
        when (length nsField /= length (L.nub nsField))
         $ throwError $ ErrorClashField path nsField


-------------------------------------------------------------------------------
-- | Check that a tree is well formed.
checkTree :: Tree -> Either Error ()
checkTree tree 
 =      checkTree' mempty tree


-- | Check that a tree is well formed, at the given starting path.
checkTree' :: Path -> Tree -> Either Error ()
checkTree' path (Tree branch branchType)
 =      checkBranch path branch branchType


-- | Check that a branch has the given branch type.
checkBranch :: Path -> Branch -> BranchType -> Either Error ()
checkBranch
        (Path ps pts) 
        (B key subs) bt@(BT name tKey@(TT nts) tsSub)
 = do
        let ps'         = IForest name : ps
        let pts'        = ITForest bt  : pts
        let path'       = Path ps' pts'
        let tPath'      = PathType pts'

        -- Check the tuple type.
        checkKeyType tPath' tKey 

        -- Check the key matches its type.
        checkTuple   path'  key tKey

        -- Check that the number of sub trees matches the number of
        -- sub dimensions.
        when (length subs /= length tsSub)
         $ throwError $ ErrorArityDim path' subs tsSub

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | BT n _ _ <- tsSub]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim tPath' nsSub

        -- Check each of the sub trees.
        zipWithM_ (checkGroup path') subs tsSub


-------------------------------------------------------------------------------
-- | Check that a forest is well formed.
checkForest  :: Forest -> Either Error ()
checkForest forest
        = checkForest' mempty forest


-- | Check that a forest is well formed, at the given starting path.
checkForest' :: Path -> Forest -> Either Error ()
checkForest' path (Forest bs bt)
        = checkGroup path bs bt


-- | Check that all the branches in a group
--   have the specified shape.
checkGroup :: Path -> Group -> BranchType -> Either Error ()
checkGroup path (G bs) shape
 = do   zipWithM_ 
                (\i b -> checkBranch path b shape)
                [0..] bs


-------------------------------------------------------------------------------
-- | Check that a key is well formed.
checkKey  :: Key -> Either Error ()
checkKey key
        = checkKey' mempty key


-- | Check that a key is well formed, at the given starting path.
checkKey' :: Path -> Key -> Either Error ()
checkKey' path (Key t tt)
        = checkTuple path t tt


-- | Check that a tuple has the given type.
checkTuple :: Path -> Tuple -> TupleType -> Either Error ()
checkTuple path@(Path ps pts) (T fields) tt@(TT nts)
 = do   
        -- Check that the number of fields matches the tuple type.
        when (length fields /= length nts)
         $ throwError $ ErrorArityTuple path fields nts

        zipWithM_ 
                (\  field (name, tField)
                 ->     checkAtom path field tField)
                fields nts


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


-------------------------------------------------------------------------------
-- | Possible type errors.
data Error
        -- | Number of sub trees does not match number of sub dimensions.
        = ErrorArityDim         Path [Group] [BranchType] 

        -- | Number of fields in tuple does not match tuple type.
        | ErrorArityTuple       Path [Atom]  [(Name, AtomType)]

        -- | Sub dimension name clash.
        | ErrorClashSubDim      PathType [Name]

        -- | Field name clash.
        | ErrorClashField       PathType [Name]

        -- | Atomic value does not match associated type.
        | ErrorAtom             Path Atom    AtomType
        deriving Show

