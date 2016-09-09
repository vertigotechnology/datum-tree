
module Datum.Data.Tree.Operator.Push
        ( pushDimOfForest)
where
import Datum.Data.Tree.Exp


pushDimOfForest 
        :: Name 
        -> [Key 'O] 
        -> Forest 'O 
        -> Forest 'O

pushDimOfForest nDimDst keys 
        (Forest g0@(G _on0 _bs0) bt0@(BT _n0 _tt0 _bts0))
 = let
        tsK     = [ t  | Key t _  <- keys]
        ttK : _ = [ tt | Key _ tt <- keys]

   in   Forest  (G  None (boxes [B t (boxes [g0]) | t <- tsK]))
                (BT nDimDst ttK (boxes [bt0]))

