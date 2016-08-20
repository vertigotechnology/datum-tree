
module Datum.Data.Tree.Operator.Fields
        (HasFields (..))
where
import Datum.Data.Tree.Exp
import Data.Maybe
import qualified Data.Repa.Array        as A


class HasFields a where
 -- | Take the field names of an object.
 takeFieldNames :: a -> [Name]

 -- | Rename the fields of an object.
 -- 
 --   * If there are less new names than there are fields in the thing
 --     then the initial ones are renamed and the rest are preserved.
 --
 --   * If there are more new names then the excess ones are dropped.
 -- 
 renameFields   :: [Name] -> a -> a

 
 -- | Permute the fields of an object.
 --
 --   * Field are selected from the object in turn.
 --     It's ok to repeat field names, or leave some out.
 permuteFields  :: [Name] -> a -> a


-------------------------------------------------------------------------------
instance HasFields TupleType where
 takeFieldNames (TT nats)
        = [ n | Box n :*: _ <- A.toList nats]


 renameFields ns' (TT nats)
  = let 
        (ns, ats) 
         = unzip $ [(n, at) | Box n :*: Box at <- A.toList nats]

        len       = length ns

        ns_init'  = take len ns'
        ns_tail   = drop (length ns_init') ns

        ns_new    = ns_init' ++ ns_tail

        nats'     = A.fromList
                  $ [Box n :*: Box at   
                        | n  <- ns_new
                        | at <- ats    ]
    in  TT nats'        


 permuteFields ns' (TT nats)
  = let natsList
         = [(n, at) 
                | Box n :*: at <- A.toList nats]

        nats'   
         = A.fromList
         $ mapMaybe
            (\n -> case lookup n natsList of
                        Just at'        -> Just $ Box n :*: at'
                        _               -> Nothing)
         $ ns'

    in  TT nats'


-------------------------------------------------------------------------------
instance HasFields BranchType where
 renameFields ns' (BT n tt bts)
        = BT n (renameFields ns' tt) bts

 takeFieldNames (BT _ tt _)
        = takeFieldNames tt

 permuteFields ns (BT n tt bts)
        = BT n (permuteFields ns tt) bts


-------------------------------------------------------------------------------
instance HasFields (Tree c) where
 takeFieldNames    (Tree _ bt)
        = takeFieldNames bt

 renameFields ns'  (Tree b bt)
        = Tree   b (renameFields ns' bt)

 permuteFields ns  (Tree (B (T as) gs) (BT name (TT nats) bts))
  = let natsList
         = [(n, (at, i))
                | Box n :*: at  <- A.toList nats
                | i             <- [0..]]

        nats'
         = A.fromList
         $ mapMaybe 
            (\n -> case lookup n natsList of
                        Just (at', _)   -> Just $ Box n :*: at'
                        _               -> Nothing)
         $ ns

        asList = unboxes as
        as'
         = boxes
         $ mapMaybe
            (\n -> case lookup n natsList of
                        Just (_, i')    -> Just $ asList !! i'
                        _               -> Nothing)
         $ ns


    in  Tree (B (T as') gs) (BT name (TT nats') bts)


-------------------------------------------------------------------------------
instance HasFields (Forest c) where
 takeFieldNames    (Forest _ bt)
        = takeFieldNames bt

 renameFields ns'  (Forest g bt)
        = Forest g (renameFields ns' bt)

 permuteFields ns' (Forest (G n bs) bt)
  = let -- TODO: if a particular tuple does not have the required name,
        -- then the type of tuples in the result may be different.
        -- We need to weaken their types.
        bs'     = A.map (\(Box b) -> Box (permuteFieldsOfBranch ns' bt b)) bs
        bt'     = permuteFields ns' bt
    in  Forest (G n bs') bt'


permuteFieldsOfBranch ns (BT _ (TT nats) _) (B (T as) gs) 
  = let natsList
         = [(n, (at, i))
                | Box n :*: at  <- A.toList nats
                | i             <- [0..]]

        asList = unboxes as
        as'
         = boxes
         $ mapMaybe
            (\n -> case lookup n natsList of
                        Just (_, i')    
                         | i' >= length asList  -> Nothing
                         | otherwise            -> Just $ asList !! i'
                        _                       -> Nothing)
         $ ns


    in  B (T as') gs

