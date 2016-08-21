
module Datum.Script.Core.Eval.Prim.Sort
        (step_Sort)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Data.Tree.Operator.Sort
import qualified Datum.Data.Tree.Exp            as T
import qualified Data.Text                      as Text

step_Sort _ _ PPSortByField 
        [ VName  name
        , VForest forest@(T.Forest _ (T.BT _ tt _)) ]
 = do
        let compareTuple t1 t2
                = let k1 = T.Key t1 tt
                      k2 = T.Key t2 tt
                  in  compare (T.elementOfKey (Text.unpack name) k1) 
                              (T.elementOfKey (Text.unpack name) k2)

        let forest' = sortForest compareTuple forest
        progress $ VForest forest'

step_Sort _ _ _ _
 =      crash