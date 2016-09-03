
-- | Reflect interpreter evaluations back into Haskell.
module Datum.Script.Core.Eval.Reflect
        ( reflectKeyTransform
        , reflectTreeTransform
        , reflectForestTransform)
where
import Datum.Script.Core.Eval.Env
import Datum.Script.Core.Eval.State
import Datum.Script.Core.Eval.Error
import Datum.Script.Core.Exp
import Datum.Data.Tree.Codec.Matryo.Decode              ()
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Exp                    as T
import qualified Datum.Script.Core.Eval.Env             as Env
import qualified Data.Repa.Array                        as A
import qualified Text.Show.Pretty                       as Text
import qualified Data.Text                              as Text


type Reflection a
        = (State -> IO (Either Error (Maybe State)))
                                -- ^ Interpreter transition function.
        -> State                -- ^ Starting state for interpreter.
        -> Value                -- ^ Thunk to apply.
        -> T.Path               -- ^ Current path in overall tree.
        -> a                    -- ^ Value to transform.
        -> IO a                 -- ^ Transformed value.


---------------------------------------------------------------------------------------------------
-- | Reflect evaluation of a key transform back into Haskell.
reflectKeyTransform :: Reflection (T.Key 'T.O)
reflectKeyTransform sstep state0 thunk _path0 key0
 = do
        let pdRecord = PDRecord (fieldsOfKey key0)

        state'  <- runMachine sstep
                $  setStateApp thunk (PVData pdRecord) state0

        case stateControl state' of
         ControlPAP (PAF (PVData (PDRecord fields)) [])
               -> return (keyOfFields fields)

         focus -> error $ "datum-tree: unexpected result in tree evaluation."
               ++ Text.ppShow focus


-- | Convert a key to a list of record fields.
fieldsOfKey :: T.Key 'T.O -> [PrimField Exp]
fieldsOfKey key0
 = let  T.Key (T.T arrAtoms) (T.TT arrNameTypes)
                = key0

        atoms   = T.unboxes arrAtoms

        (names, tys) 
                = unzip [  (name, ty) 
                        |  T.Box name T.:*: T.Box ty 
                        <- A.toList arrNameTypes]

   in   [ PFField (Text.pack name) 
                  (Just (XFrag (PTType (PTAtom ty))))
                  (PDAtom atom)
        | name  <- names
        | ty    <- tys
        | atom  <- atoms ]


-- | Convert a list of record fields to a key.
keyOfFields  :: [PrimField Exp] -> T.Key 'T.O
keyOfFields fs
 = let  
        names   = map (Text.unpack . pffieldName) fs

        tys     = map (\f -> case pffieldType f of
                                Just (XFrag (PTType (PTAtom ty)))  
                                                -> ty
                                _               -> error "keyOfFields: bad type")
                $ fs

        atoms   = map (\f -> case pffieldValue f of
                                PDAtom atom     -> atom
                                _               -> error "keyOfField: not an atom")
                $ fs

   in   T.Key   (T.T  (T.boxes atoms))
                (T.TT (A.fromList [ T.Box name T.:*: T.Box ty
                            | name      <- names
                            | ty        <- tys]))


---------------------------------------------------------------------------------------------------
-- | Reflect evaluation of a tree transform back into Haskell.
-- 
--   Given the transition function and starting state for the interpreter,
--   take a thunk that encodes a tree transform and produce a 
--   native Haskell tree transformer.
--
reflectTreeTransform :: Reflection (T.Tree 'T.O)
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
-- | Reflect evaluation of a forest transform back into Haskell.
-- 
--   Given the transition function and starting state for the interpreter,
--   take a thunk that encodes a forest transform and produce a 
--   native Haskell forest transformer.
--
reflectForestTransform :: Reflection (T.Forest 'T.O)
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
          -> error $ unlines
                   [ show err
                   , Text.ppShow state ]

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

