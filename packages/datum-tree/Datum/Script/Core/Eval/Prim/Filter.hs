
module Datum.Script.Core.Eval.Prim.Filter
        (step_Filter)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Script.Core.Eval.Reflect
import Datum.Data.Tree
import Data.IORef
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Exp                    as T
import qualified Datum.Data.Tree.Operator.Cast          as T
import qualified System.IO.Unsafe                       as System
import qualified Data.Text      as Text



step_Filter self state
        PPFilterKeys
        [ thunk@VClo{}
        , VForest forest0]
 = do

        -- Ref to stash errors we might get when running the interpreter.
        refError <- newIORef Nothing

        -- Reflect key predicate back to Haskell.
        let inception :: T.Path -> T.Tree 'T.O -> Bool
            inception path (T.Tree (T.B t _) (T.BT _ tt _))
                = System.unsafePerformIO
                $ do    result <- reflectKeyPredicate
                                        self state { stateContext = ContextNil }
                                        thunk path (T.Key t tt)
                        case result of
                         Left err       
                          -> do writeIORef refError (Just err)
                                return True

                         Right b'
                          ->    return b'

        -- Evaluate the new forest.
        --   NOTE: The evaluation here needs to be strict to force evaluation
        --   and grimely write any errors into the IORef that we read in the
        --   next action.
        let !forest'
                = T.promiseForest
                $ T.filterTrees' inception mempty forest0

        -- Check for error.
        mError  <- readIORef refError
        case mError of
         Just err -> failure err
         Nothing  -> progress $ VForest forest'


step_Filter _ _
        PPDropDim 
        [VName name, VForest forest]
 =      
        progress $ VForest
                 $ dropDimOfForest (Text.unpack name) forest

step_Filter _ state _ _
 = crash state