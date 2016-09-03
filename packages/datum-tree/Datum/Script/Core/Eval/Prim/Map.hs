
module Datum.Script.Core.Eval.Prim.Map
        (step_Map)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Script.Core.Eval.Reflect
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Operator.Cast          as T
import qualified System.IO.Unsafe                       as System


step_Map self state PPMapKeys [thunk@VClo{}, VForest forest0]
 = do
        let inception :: T.Path -> T.Key 'T.O -> T.Key 'T.O
            inception path key
                = T.promiseKey
                $ System.unsafePerformIO
                $ reflectKeyTransform 
                        self state { stateContext = ContextNil }
                        thunk path key

        progress $ VForest
                 $ T.promiseForest
                 $ T.mapKeysOfForest 
                        inception mempty forest0

step_Map _ _ _ _
 = crash
