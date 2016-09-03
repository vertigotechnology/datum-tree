
module Datum.Script.Core.Eval.Prim.Traverse
        (step_Traverse)
where
import Datum.Script.Core.Eval.Reflect
import Datum.Script.Core.Eval.Prim.Base
import Datum.Data.Tree.Codec.Matryo.Decode              ()
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Operator.Cast          as T

import qualified System.IO.Unsafe                       as System

import qualified Text.Show.Pretty                       as Text


-- Tree Traversal ---------------------------------------------------
-- | Apply a per-tree function to the trees at the given path.
step_Traverse self state PPAt [VArray _ names, thunk, VTree tree0]
  = do  let Just names' 
                = sequence
                $ map takePDName names

        -- A dream within a dream.
        --   This function calls our interpreter to apply the thunk.
        --   The interface is via a pure Haskell function,
        --   so when we pass it to the Haskell-side operators they
        --   don't know they're calling back into the interpreter.
        let inception :: T.Path -> T.Tree 'T.O -> T.Tree 'T.O
            inception path tree
                = T.promiseTree
                $ System.unsafePerformIO
                $ reflectTreeTransform 
                        self  state { stateContext = ContextNil }
                        thunk path tree

        progress $ VTree
                 $ T.promiseTree
                 $ T.mapTreesOfTreeOn
                        names' inception
                        mempty tree0


-- | Apply a per-forest function to the tree at the given path.
step_Traverse self state PPOn [VArray _ names, thunk, VTree tree0]
  = do  let Just names' 
                = sequence
                $ map takePDName names

        -- A dream within a dream.
        --   This function calls our interpreter to apply the thunk.
        --   The interface is via a pure Haskell function,
        --   so when we pass it to the Haskell-side operators they
        --   don't know they're calling back into the interpreter.
        let inception :: T.Path -> T.Forest 'T.O -> T.Forest 'T.O
            inception path forest
                = T.promiseForest
                $ System.unsafePerformIO
                $ reflectForestTransform 
                        self  state { stateContext = ContextNil }
                        thunk path forest

        progress $ VTree
                 $ T.promiseTree
                 $ T.mapForestOfTreeOn
                        names' inception
                        mempty tree0

step_Traverse _ _ p vs
 = error (Text.ppShow (p, vs))

        --crash


