
module Datum.Schema.Check
        ( checkShape
        , checkTypeTup
        , checkTree
        , checkTup
        , checkLit)
where
import Datum.Schema.Exp
import Datum.Schema.Type
import Control.Monad
import Control.Monad.Except
import qualified Data.List              as L


-------------------------------------------------------------------------------
-- | Check that a shape is well formed.
checkShape :: Path -> Shape -> Either Error ()
checkShape path (TS name tKey subs)
 = do
        -- Check the tuple type.
        checkTypeTup path tKey

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | TS n _ _ <- subs]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim path nsSub

        -- Check the sub dimension shapes.
        mapM_ (checkShape (IxSub name : path)) subs


-- | Check that a tuple type is well formed.
checkTypeTup :: Path -> TypeTup -> Either Error ()
checkTypeTup path (TT nts)
 = do
        -- Check that field names do not clash.
        let nsField     = [n | (n, _)   <- nts]
        when (length nsField /= length (L.nub nsField))
         $ throwError $ ErrorClashField path nsField


-------------------------------------------------------------------------------
-- | Check that a tree has the specified shape.
checkTree :: Path -> Tree -> Shape -> Either Error ()
checkTree path (Tree key subs) (TS name tKey@(TT nts) tsSub)
 = do
        -- Check the tuple type.
        checkTypeTup path tKey 

        -- Check the key matches its type.
        checkTup path key tKey

        -- Check that the number of sub trees matches the number of
        -- sub dimensions.
        when (length subs /= length tsSub)
         $ throwError $ ErrorArityDim path subs tsSub

        -- Check that sub dimension names do not clash.
        let nsSub       = [n | TS n _ _ <- tsSub]
        when (length nsSub /= length (L.nub nsSub))
         $ throwError $ ErrorClashSubDim path nsSub

        -- Check each of the sub trees.
        zipWithM_ (checkTrees (IxSub name : path)) subs tsSub


-- | Check that a tree group has the specified shape.
checkTrees :: Path -> [Tree] -> Shape -> Either Error ()
checkTrees path trees shape
 = do   zipWithM_ 
                (\i tree 
                 ->     checkTree (IxElem i : path) tree shape)
                [0..] trees


-- | Check that a tuple has the given type.
checkTup  :: Path -> Tup -> TypeTup -> Either Error ()
checkTup path (Tup fields) (TT nts)
 = do   
        -- Check that the number of fields matches the tuple type.
        when (length fields /= length nts)
         $ throwError $ ErrorArityTuple path fields nts

        zipWithM_ 
                (\  field (name, tField)
                 ->     checkLit (IxField name : path) field tField)
                fields nts


-- | Check that a literal has the givne type.
checkLit  :: Path -> Lit -> TypePrim -> Either Error ()
checkLit path lit tp
 = case (lit, tp) of
        (LUnit,         TPUnit)         -> return ()
        (LBool _,       TPBool)         -> return ()
        (LInt _,        TPInt)          -> return ()
        (LFloat _,      TPFloat)        -> return ()
        (LNat _,        TPNat)          -> return ()
        (LDecimal _,    TPDecimal)      -> return ()
        (LText _,       TPText)         -> return ()
        (LTime _,       TPTime)         -> return ()
        _ -> throwError $ ErrorLiteral path lit tp


-------------------------------------------------------------------------------
-- | Possible type errors.
data Error
        -- | Number of sub trees does not match number of sub dimensions.
        = ErrorArityDim         Path [[Tree]]   [Shape] 

        -- | Sub dimension name clash.
        | ErrorClashSubDim      Path [Name]

        -- | Number of fields in tuple does not match tuple type.
        | ErrorArityTuple       Path [Lit]      [(Name, TypePrim)]

        -- | Field name clash.
        | ErrorClashField       Path [Name]

        -- | Literal value does not match associated type.
        | ErrorLiteral          Path Lit        TypePrim
        deriving Show

