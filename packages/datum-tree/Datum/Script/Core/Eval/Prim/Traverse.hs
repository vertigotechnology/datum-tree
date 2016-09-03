
module Datum.Script.Core.Eval.Prim.Traverse
        (step_Traverse)
where
import Datum.Script.Core.Eval.Reflect
import Datum.Script.Core.Eval.Prim.Base
import Datum.Data.Tree.Codec.Matryo.Decode              ()
import Data.IORef
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Operator.Cast          as T
import qualified System.IO.Unsafe                       as System


-------------------------------------------------------------------------------
-- | Apply a per-tree function to the trees at the given path.
step_Traverse self state PPAt [VArray _ names, thunk, VTree tree0]
  = do  
        -- Names for the path that we're applying the transform at.
        let Just names' 
                = sequence
                $ map takePDName names

        -- Ref to stash errors we might get when running the interpreter.
        refError <- newIORef Nothing

        -- A dream within a dream.
        --   This function calls our interpreter to apply the thunk.
        --   The interface is via a pure Haskell function,
        --   so when we pass it to the Haskell-side operators they
        --   don't know they're calling back into the interpreter.
        let inception :: T.Path -> T.Tree 'T.O -> T.Tree 'T.O
            inception path tree
                = T.promiseTree
                $ System.unsafePerformIO
                $ do    result <- reflectTreeTransform 
                                        self  state { stateContext = ContextNil }
                                        thunk path tree

                        case result of
                         Left err
                          -> do writeIORef refError (Just err)
                                return tree

                         Right tree'
                          ->    return tree'

        -- Evaluate the new forest.
        --   NOTE: The evaluation here needs to be strict to force evaluation
        --   and grimely write any errors into the IORef that we read in the
        --   next action.
        let !tree'
                = T.promiseTree
                $ T.mapTreesOfTreeOn names' inception mempty tree0

        -- Check for error.
        mError  <- readIORef refError
        case mError of
         Just err -> failure err
         Nothing  -> progress $ VTree tree'
                              

-------------------------------------------------------------------------------
-- | Apply a per-forest function to the tree at the given path.
step_Traverse self state PPOn [VArray _ names, thunk, VTree tree0]
  = do  
        -- Names for the path that we're applying the transform at.
        let Just names' 
                = sequence
                $ map takePDName names


        -- Ref to stash errors we might get when running the interpreter.
        refError <- newIORef Nothing

        -- A dream within a dream.
        --   This function calls our interpreter to apply the thunk.
        --   The interface is via a pure Haskell function,
        --   so when we pass it to the Haskell-side operators they
        --   don't know they're calling back into the interpreter.
        let inception :: T.Path -> T.Forest 'T.O -> T.Forest 'T.O
            inception path forest
                = T.promiseForest
                $ System.unsafePerformIO
                $ do    result  <- reflectForestTransform 
                                        self  state { stateContext = ContextNil }
                                        thunk path forest

                        case result of
                         Left err
                          -> do writeIORef refError (Just err)
                                return forest

                         Right forest'
                          ->    return forest'

        -- Evaluate the new tree.
        --   NOTE: The evaluation here needs to be strict to force evaluation
        --   and grimely write any errors into the IORef that we read in the
        --   next action.
        let !tree' 
                = T.promiseTree
                $ T.mapForestOfTreeOn names' inception mempty tree0

        -- Check for error.
        mError  <- readIORef refError
        case mError of
         Just err -> failure err
         Nothing  -> progress $ VTree tree'

step_Traverse _ state _ _
 = crash state



