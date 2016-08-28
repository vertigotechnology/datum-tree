
module Datum.Data.Tree.Operator.Group
        (groupForest)
where
import Datum.Data.Tree.Exp
import qualified Data.HashMap.Strict    as H
import qualified Data.Sequence          as S
import qualified Data.Foldable          as S
import qualified Data.Repa.Array        as A
import qualified Data.List              as L


-- | Group the trees in a forest by the field that has the given name.
groupForest :: Name -> Forest c -> Forest c
groupForest name f@(Forest (G onGroup bs) (BT nBranch tt ts))
 -- Get the index of the tuple field that we're grouping on.
 | Just ixField  <- indexOfField name tt
 , Just tyField  <- typeOfField  name tt
 = let
        -- Insert all the branches into the hashmap based on their key.
        insert  :: H.HashMap Atom (S.Seq Branch)
                -> Branch
                -> H.HashMap Atom (S.Seq Branch)

        insert !hm (B t@(T as) gs)
         = let  Box a
                  = if ixField >= A.length as
                        then error "groupForest: tuple length mismatch"
                        else A.index as ixField

                b' = B (deleteFieldOfTuple ixField t) gs

           in   H.insertWith (S.><) a (S.singleton b') hm

        -- Build branches for each of the resulting groups.
        bs'     = [ B (T (A.singleton (Box a)))
                      (A.singleton    (Box (G None (boxes $ S.toList sbs))))
                  | (a, sbs) <- H.toList $ L.foldl' insert H.empty $ reverse $ unboxes bs ]

        -- Update the branch type to reflect that we've removed
        -- the grouping field from the tuples.
        tt'     = deleteFieldOfTupleType ixField tt
        bt'     = BT nBranch tt' ts

   in   Forest  (G  onGroup (boxes bs'))
                (BT name (TT (A.singleton (Box name :*: Box tyField)))
                             (A.singleton (Box bt')))


 -- If the named field does not exist then return the original forest.
 | otherwise
 = f


-- | Lookup the index of a named field from a tuple type.
indexOfField :: Name -> TupleType -> Maybe Int
indexOfField name (TT fs)
 = let  ns      = [n | Box n :*: _ <- A.toList fs]
   in   lookup name $ zip ns [0..]


-- | Lookup the type of a named field from a tuple type.
typeOfField  :: Name -> TupleType -> Maybe AtomType
typeOfField name (TT fs)
 = let  ats     = [(n, at) | Box n :*: Box at <- A.toList fs]
   in   lookup name ats


-- | Delete the field with the given index from a tuple.
deleteFieldOfTuple :: Int -> Tuple -> Tuple
deleteFieldOfTuple i (T as)
        = T $ boxes
        $ map snd
        $ filter (\(j, _) -> j /= i)
        $ [ (j, a)  | a <- unboxes as
                    | j <- [0..] ]


-- | Delete the field with the given index from a tuple type.
deleteFieldOfTupleType :: Int -> TupleType -> TupleType
deleteFieldOfTupleType i (TT fs)
        = TT $ A.fromList
        $ map snd
        $ filter (\(j, _) -> j /= i)
        $ [ (j, a)  | a <- A.toList fs
                    | j <- [0..] ]
