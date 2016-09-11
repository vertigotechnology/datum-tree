
module Datum.Script.Core.Eval.Prim.Reduce
        (step_Reduce)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Script.Core.Eval.Reflect
import Datum.Data.Tree
import Data.IORef
import qualified Data.Text                       as Text
import qualified System.IO.Unsafe                as System
import qualified Datum.Data.Tree                 as T
import qualified Datum.Data.Tree.Operator.Cast   as T
import qualified Datum.Data.Tree.Operator.Strip  as T


step_Reduce _ _
        PPCountAsField 
        [VName nField, VName nSub, VTree tree]
 =      progress $ VTree
                 $ countAsFieldTree 
                        (Text.unpack nField)
                        (Text.unpack nSub) 
                        tree


step_Reduce _ _
        PPCountAsField
        [VName nField, VName nSub, VForest forest]
 =      progress $ VForest
                 $ countAsFieldForest
                        (Text.unpack nField)
                        (Text.unpack nSub) 
                        forest

step_Reduce self state
        PPFoldAsField
        [VName nField, VName nSub, thunk, VAtom aZero, VForest forest0]
 = do
        -- Ref to stash errors we might get when running the interpreter.
        refError <- newIORef Nothing

        -- A dream within a dream.
        --   This function calls our interpreter to apply the thunk.
        --   The interface is via a pure Haskell function,
        --   so when we pass it to the Haskell-side operators they
        --   don't know they're calling back into the interpreter.
        let inception 
                :: T.Path -> T.Atom -> T.Tree 'T.O -> T.Atom

            inception path aAcc tree
                = System.unsafePerformIO
                $ do    result  <- reflectKeyFoldWorker 
                                        self  state { stateContext = ContextNil }
                                        thunk path (aAcc, T.strip tree)

                        case result of
                         Left err
                          -> do writeIORef refError (Just err)
                                return aAcc

                         Right aAcc'
                          ->    return aAcc'   

        -- Evaluate the new tree.
        --   NOTE: The evaluation here needs to be strict to force evaluation
        --   and grimely write any errors into the IORef that we read in the
        --   next action.
        let !forest' 
                = T.promiseForest
                $ T.foldAsFieldForest
                        (Text.unpack nField)
                        (Text.unpack nSub)
                        inception mempty aZero forest0

        -- Check for error.
        mError <- readIORef refError
        case mError of
         Just err   -> failure err
         Nothing    -> progress $ VForest forest'


step_Reduce _ state _ _
 = crash state


