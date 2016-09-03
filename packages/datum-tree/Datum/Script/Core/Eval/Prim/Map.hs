
module Datum.Script.Core.Eval.Prim.Map
        (step_Map)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Script.Core.Eval.Reflect
import Data.IORef
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Operator.Cast          as T
import qualified System.IO.Unsafe                       as System


step_Map self state PPMapKeys [thunk@VClo{}, VForest forest0]
 = do
        -- Ref to stash errors we might get when running the interpreter.
        refError <- newIORef Nothing

        -- Reflect interpreted key transform back to Haskell.
        let inception :: T.Path -> T.Key 'T.O -> T.Key 'T.O
            inception path key
                = T.promiseKey
                $ System.unsafePerformIO
                $ do    result <- reflectKeyTransform 
                                        self state { stateContext = ContextNil }
                                        thunk path key
                        case result of
                         Left err       
                          -> do writeIORef refError (Just err)
                                return key

                         Right key'
                          ->    return key'

        -- Evaluate the new forest.
        --   NOTE: The evaluation here needs to be strict to force evaluation
        --   and grimely write any errors into the IORef that we read in the
        --   next action.
        let !forest'
                = T.promiseForest
                $ T.mapKeysOfForest inception mempty forest0

        -- Check for error.
        mError  <- readIORef refError
        case mError of
         Just err -> failure err
         Nothing  -> progress $ VForest forest'

step_Map _ _ _ _
 = crash
