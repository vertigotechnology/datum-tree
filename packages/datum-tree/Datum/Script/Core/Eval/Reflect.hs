
-- | Reflect interpreter evaluations back into Haskell.
module Datum.Script.Core.Eval.Reflect
        ( reflectForestTransform
        , reflectTreeTransform)
where
import Datum.Script.Core.Eval.Env
import Datum.Script.Core.Eval.State
import Datum.Script.Core.Eval.Error
import Datum.Script.Core.Exp
import Datum.Data.Tree.Codec.Matryo.Decode              ()
import qualified Datum.Data.Tree                        as T
import qualified Datum.Script.Core.Eval.Env             as Env
import qualified Text.Show.Pretty                       as Text


---------------------------------------------------------------------------------------------------
-- | Reflect evaluation of a forest transform back into Haskell.
-- 
--   Given the transition function and starting state for the interpreter,
--   take a thunk that encodes a forest transform and produce a 
--   native Haskell forest transformer.
--
reflectForestTransform
        :: (State -> IO (Either Error (Maybe State)))
                                -- ^ Interpreter transition function.        
        -> State                -- ^ Starting state for interpreter.
        -> Value                -- ^ Thunk that maps forests to forests.
        -> T.Path               -- ^ Current path in the overall tree.
        -> T.Forest 'T.O        -- ^ Forest to transform.
        -> IO (T.Forest 'T.O)   -- ^ Transformed forest.

reflectForestTransform sstep state0 thunk _path0 forest0
 = do   
        state'  <- runMachine sstep
                $  setStateApp thunk (PVData (PDForest forest0)) state0

        case stateControl state' of
         -- Expression normalized to a forest, ok.
         ControlPAP (PAF (PVData (PDForest f)) [])
               -> return f

         -- Normalized expression was not a forest.
         -- The thunk that was provided does not map forests to forests.
         focus -> error $ "datum-tree: wrong type in internal forest evaluation "
               ++ Text.ppShow focus


---------------------------------------------------------------------------------------------------
-- | Reflect evaluation of a tree transform back into Haskell.
-- 
--   Given the transition function and starting state for the interpreter,
--   take a thunk that encodes a tree transform and produce a 
--   native Haskell tree transformer.
--
reflectTreeTransform
        :: (State -> IO (Either Error (Maybe State)))
                                -- ^ Interpreter transition function.
        -> State                -- ^ Starting state for interpreter.
        -> Value                -- ^ Thunk that maps trees to trees.
        -> T.Path               -- ^ Current path in the overall tree.
        -> T.Tree 'T.O          -- ^ Tree to transform.
        -> IO (T.Tree 'T.O)     -- ^ Transformed tree.

reflectTreeTransform sstep state0 thunk _path0 tree0
 = do   
        state'  <- runMachine sstep 
                $  setStateApp thunk (PVData (PDTree tree0)) state0

        case stateControl state' of
         -- Expression normalized to a tree, ok.
         ControlPAP (PAF (PVData (PDTree t)) [])
               -> return t

         -- Normalized expression is not a tree.
         -- The thunk that was provided does not map trees to trees.
         focus -> error $ "datum-tree: unexpected result in tree evaluation."
               ++ Text.ppShow focus


---------------------------------------------------------------------------------------------------
-- | Run the interpreter until it stops stepping.
runMachine  
        :: (State -> IO (Either Error (Maybe State)))
        ->  State
        ->  IO State

runMachine sstep state
 = do   
        -- Take a single step.
        result  <- sstep state

        case result of
         -- Interpretation crashed.
         Left err               
          -> error $ show err

         -- Transitioned to a new state.
         Right (Just state')
          -> runMachine sstep state'

         -- Interpreter has normalized the expression.
         Right Nothing
          -> return state


---------------------------------------------------------------------------------------------------
-- | Apply thunk to an argument and set the result as the new control
--   in the given state.
setStateApp :: Value -> Frag -> State -> State
setStateApp thunk arg state
 = case curryThunkPrim thunk arg of
        VClo (Clo x1 env)
         -> state { stateEnv     = env
                  , stateControl = ControlExp x1 }

        VPAP pap 
         -> state { stateEnv     = Env.empty
                  , stateControl = ControlPAP pap }


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

