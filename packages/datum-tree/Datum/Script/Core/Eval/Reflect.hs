
-- | Reflect interpreter evaluations back into Haskell.
module Datum.Script.Core.Eval.Reflect
        ( reflectKeyTransform
        , reflectKeyPredicate
        , reflectTreeTransform
        , reflectForestTransform
        , reflectKeyFoldWorker
        , keyOfFields)
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
import qualified Data.Text                              as Text


type Reflection a b
        = (State -> IO (Either Error (Maybe State)))
                                -- ^ Interpreter transition function.
        -> State                -- ^ Starting state for interpreter.
        -> Value                -- ^ Thunk to apply.
        -> T.Path               -- ^ Current path in overall tree.
        -> a                    -- ^ Value to transform.
        -> IO (Either Error b)  -- ^ Transformed value.


---------------------------------------------------------------------------------------------------
-- | Reflect evaluation of a key transform back into Haskell.
reflectKeyTransform 
        :: Reflection (T.Key 'T.O) (T.Key 'T.O)

reflectKeyTransform sstep state0 thunk _path0 key0
 = do
        let pdRecord = PDRecord (fieldsOfKey key0)

        result' <- runMachine sstep
                $  setStateApp thunk [PVData pdRecord] state0

        case result' of
         Left  err       
          -> return $ Left err

         Right state'
          -> case stateControl state' of
                ControlPAP (PAF (PVData (PDRecord fields)) [])
                  -> return $ Right $ keyOfFields fields
                _ -> return $ Left  $ Error "Unexpected result in tree evaluation."


-- | Reflect evaluation of a key predicate back into Haskell.
reflectKeyPredicate
        :: Reflection (T.Key 'T.O) Bool

reflectKeyPredicate sstep state0 thunk _path0 key0
 = do
        let pdRecord = PDRecord (fieldsOfKey key0)

        result' <- runMachine sstep
                $  setStateApp thunk [PVData pdRecord] state0

        case result' of
         Left  err       
          -> return $ Left err

         Right state'
          -> case stateControl state' of
                ControlExp (XFrag (PVData (PDAtom (ABool b))))
                  -> return $ Right b

                ControlPAP (PAF   (PVData (PDAtom (ABool b))) [])
                  -> return $ Right b

                c -> return $ Left  $ Error 
                            $ "Unexpected result in key preficate." ++ show c




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
reflectTreeTransform 
        :: Reflection (T.Tree 'T.O) (T.Tree 'T.O)

reflectTreeTransform sstep state0 thunk _path0 tree0
 = do   
        result  <- runMachine sstep 
                $  setStateApp thunk [PVData (PDTree tree0)] state0

        case result of
         Left err
          -> return $ Left err

         Right state'
          -> case stateControl state' of
                -- Expression normalized to a tree, ok.
                ControlPAP (PAF (PVData (PDTree t)) [])
                  -> return $ Right t

                -- Normalized expression is not a tree.
                -- The thunk that was provided does not map trees to trees.
                _ -> return $ Left $ Error "Unexpected result in tree evaluation."


---------------------------------------------------------------------------------------------------
-- | Reflect evaluation of a forest transform back into Haskell.
-- 
--   Given the transition function and starting state for the interpreter,
--   take a thunk that encodes a forest transform and produce a 
--   native Haskell forest transformer.
--
reflectForestTransform 
        :: Reflection (T.Forest 'T.O) (T.Forest 'T.O)

reflectForestTransform sstep state0 thunk _path0 forest0
 = do   
        result  <- runMachine sstep
                $  setStateApp thunk [PVData (PDForest forest0)] state0

        case result of
         Left err
          -> return $ Left err

         Right state'
          -> case stateControl state' of
                -- Expression normalized to a forest, ok.
                ControlPAP (PAF (PVData (PDForest f)) [])
                  -> return $ Right f

                -- Normalized expression was not a forest.
                -- The thunk that was provided does not map forests to forests.
                _ -> return $ Left $ Error "Unexpected type in forest evaluation"


---------------------------------------------------------------------------------------------------
-- | Reflect evaluation of a tree fold worker.
reflectKeyFoldWorker
        :: Reflection (T.Atom, T.Key 'T.O) T.Atom

reflectKeyFoldWorker sstep state0 thunk _path0 (acc, key0)
 = do
        let pdRecord0 = PDRecord (fieldsOfKey key0)

        result  <- runMachine sstep
                $  setStateApp thunk 
                        [ PVData (PDAtom   acc)
                        , PVData pdRecord0 ]
                        state0

        case result of
         Left err
          -> return $ Left err

         Right state'
          -> case stateControl state' of
                -- Expression normalized to an atom, ok.
                ControlPAP (PAF (PVData (PDAtom a)) [])
                  -> return $ Right a

                -- Normalized expression was not an atom.
                _ -> return $ Left $ Error "Unexpected type in fold worker evaluation"


---------------------------------------------------------------------------------------------------
-- | Run the interpreter until it stops stepping.
runMachine  
        :: (State -> IO (Either Error (Maybe State)))
        ->  State
        ->  IO (Either Error State)

runMachine sstep state
 = do   
        -- Take a single step.
        result  <- sstep state

        case result of
         -- Interpretation crashed.
         Left err               
          -> return $ Left err

         -- Transitioned to a new state.
         Right (Just state')
          -> runMachine sstep state'

         -- Interpreter has normalized the expression.
         Right Nothing
          -> return $ Right state


---------------------------------------------------------------------------------------------------
-- | Apply thunk to an argument and set the result as the new control
--   in the given state.
setStateApp :: Value -> [Frag] -> State -> State
setStateApp thunk args state
 = case curryThunkPrim thunk args of
        VClo (Clo x1 env)
         -> state { stateEnv     = env
                  , stateControl = ControlExp x1 }

        VPAP pap 
         -> state { stateEnv     = Env.empty
                  , stateControl = ControlPAP pap }


-- | Curry the given primitive argument onto a thunk.
curryThunkPrim  :: Value -> [Frag] -> Value
curryThunkPrim tt psArg
 = case tt of
        VClo (Clo x1 env)
         -> VClo (Clo (makeXApps x1 [XFrag pArg | pArg <- psArg]) env)

        VPAP (PAP p ts)
         -> VPAP (PAP p (ts ++ [VPAP (PAF pArg []) | pArg <- psArg]))

        VPAP (PAF p ts)
         -> VPAP (PAF p (ts ++ [VPAP (PAF pArg []) | pArg <- psArg]))

