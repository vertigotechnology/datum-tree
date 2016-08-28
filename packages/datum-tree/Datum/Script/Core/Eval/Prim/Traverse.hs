
module Datum.Script.Core.Eval.Prim.Traverse
        (step_Traverse)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Data.Tree.Codec.Matryo.Decode              ()
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Operator.Cast          as T
import qualified Datum.Script.Core.Eval.Env             as Env

import qualified System.IO.Unsafe                       as System

import qualified Text.Show.Pretty                       as Text


-- Tree Traversal ---------------------------------------------------
-- | Apply a per-tree function to the trees at the given path.
step_Traverse self state PPAt [VList _ names, thunk, VTree tree0]
  = do  let Just names' 
                = sequence
                $ map takeXName names

        -- A dream within a dream.
        --   This function calls our interpreter to apply the thunk.
        --   The interface is via a pure Haskell function,
        --   so when we pass it to the Haskell-side operators they
        --   don't know they're calling back into the interpreter.
        let inception :: T.Path -> T.Tree 'T.O -> T.Tree 'T.O
            inception path tree
                = T.promiseTree
                $ System.unsafePerformIO
                $ liftTreeTransformIO self thunk 
                        state { stateContext = ContextNil }
                        path tree

        progress $ VTree
                 $ T.promiseTree
                 $ T.mapTreesOfTreeOn
                        names' inception
                        mempty tree0


-- | Apply a per-forest function to the tree at the given path.
step_Traverse self state PPOn [VList _ names, thunk, VTree tree0]
  = do  let Just names' 
                = sequence
                $ map takeXName names

        -- A dream within a dream.
        --   This function calls our interpreter to apply the thunk.
        --   The interface is via a pure Haskell function,
        --   so when we pass it to the Haskell-side operators they
        --   don't know they're calling back into the interpreter.
        let inception :: T.Path -> T.Forest 'T.O -> T.Forest 'T.O
            inception path forest
                = T.promiseForest
                $ System.unsafePerformIO
                $ liftForestTransformIO self thunk 
                        state { stateContext = ContextNil }
                        path forest

        progress $ VTree
                 $ T.promiseTree
                 $ T.mapForestOfTreeOn
                        names' inception
                        mempty tree0

step_Traverse _ _ _ _
 =      crash


---------------------------------------------------------------------------------------------------
liftForestTransformIO
        :: (State -> IO (Either Error (Maybe State)))
                        -- ^ Stepper function for general expressions.
        -> Value        -- ^ Expression mapping trees to trees.
        -> State        -- ^ Starting state.
        -> T.Path
        -> T.Forest 'T.O
        -> IO (T.Forest 'T.O)

liftForestTransformIO sstep thunk state0 _path0 forest0
 = orivour
 $ case curryThunkPrim thunk (PVData (PDForest forest0)) of
        VClo (Clo x1 env)
         -> state0 { stateEnv      = env
                   , stateControl  = ControlExp x1 }

        VPAP pap
         -> state0 { stateEnv      = Env.empty
                   , stateControl  = ControlPAP pap }

 where
        orivour state
         = do   result <- sstep state
                case result of
                 Left err       -> error $ show err
                 Right (Just state') -> orivour state'
                 Right Nothing
                  -> case stateControl state of
                        ControlPAP (PAF (PVData (PDForest t)) [])
                              -> return t
                        focus -> error $ "datum-tree: wrong type in internal forest evaluation "
                              ++ Text.ppShow focus


---------------------------------------------------------------------------------------------------
liftTreeTransformIO
        :: (State -> IO (Either Error (Maybe State)))
                        -- ^ Stepper function for general expressions.
        -> Value        -- ^ Expression mapping trees to trees.
        -> State        -- ^ Starting state.
        -> T.Path
        -> T.Tree 'T.O
        -> IO (T.Tree 'T.O)

liftTreeTransformIO sstep thunk state0 _path0 tree0
 = orivour
 $ case curryThunkPrim thunk (PVData (PDTree tree0)) of
        VClo (Clo x1 env)
         -> state0 { stateEnv     = env
                   , stateControl = ControlExp x1 }

        VPAP pap
         -> state0 { stateEnv     = Env.empty
                   , stateControl = ControlPAP pap }

 where
        orivour state
         = do   result <- sstep state
                case result of
                 Left err       -> error $ show err
                 Right (Just state') -> orivour state'
                 Right Nothing
                  -> case stateControl state of
                        ControlPAP (PAF (PVData (PDTree t)) [])
                              -> return t
                        focus -> error $ "datum-tree: unexpected type in internal tree evaluation "
                              ++ Text.ppShow focus


-- | Curry the given primitive argument onto a thunk.
curryThunkPrim  :: Value -> Frag -> Value
curryThunkPrim tt pArg
 = case tt of
        VClo (Clo x1 env)
         -> VClo (Clo (XApp x1 (XFrag pArg)) env)

        VPAP (PAP p ts)
         -> VPAP (PAP p (ts ++ [VPAP (PAF pArg [])]))

        VPAP (PAF p ts)
         -> VPAP (PAF p (ts ++ [VPAP (PAF pArg [])]))
