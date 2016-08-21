
module Datum.Data.Tree.Operator.Sort
        (sortForest)
where
import Datum.Data.Tree.Exp
import qualified System.IO.Unsafe               as System
import qualified Data.Vector                    as V
import qualified Data.Vector.Algorithms.Merge   as VA


-- | Sort the trees in a forest by their root tuples.
sortForest
        :: (Tuple -> Tuple -> Ordering)
        -> Forest 'O -> Forest 'O

sortForest compareTuple (Forest (G name bs) bt)
 = System.unsafePerformIO
 $ do   
        let bsVector    = V.fromList $ unboxes bs

        let compareBranch (B t1 _) (B t2 _)
                = compareTuple t1 t2

        tsMutable   <- V.thaw bsVector
        VA.sortBy compareBranch tsMutable
        bsVector'   <- V.freeze tsMutable

        let bs'         = boxes $ V.toList bsVector'
        return  $ Forest (G name bs') bt

