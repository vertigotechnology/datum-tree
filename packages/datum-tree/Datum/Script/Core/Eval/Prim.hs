
module Datum.Script.Core.Eval.Prim
        ( step, Error(..) )
where
import Datum.Script.Core.Eval.Error
import Datum.Script.Core.Eval.State
import Datum.Script.Core.Eval.Value
import Datum.Script.Core.Eval.Pretty
import Datum.Script.Core.Exp

import Datum.Data.Tree.Codec.Matryo.Decode              ()
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Codec                  as T
import qualified Datum.Data.Tree.Codec.Matryo.Encode    as Matryo
import qualified Datum.Data.Tree.Codec.Matryo.Decode    as Matryo
import qualified Datum.Data.Tree.Codec.SExp.Pretty      as T
import qualified Datum.Data.Tree.Operator.Cast          as T
import qualified Datum.Script.Core.Eval.Env             as Env

import qualified System.FilePath                        as FilePath
import qualified System.IO.Unsafe                       as System
import qualified System.IO                              as System

import qualified Text.PrettyPrint.Leijen                as PP
import qualified Text.Show.Pretty                       as Text

import qualified Data.ByteString.Lazy.Char8             as BS8
import qualified Data.ByteString                        as BS
import qualified Data.Text                              as Text
import qualified Data.Text.Lazy.IO                      as LText
import qualified Data.Text.Encoding                     as Text


---------------------------------------------------------------------------------------------------
-- | Evaluate a primitive applied to some arguments.
step    :: (State -> IO (Either Error (Maybe State)))
        -> State
        -> PrimOp -> [Value] -> IO (Either Error Value)


-- Numeric ----------------------------------------------------------
step _ _ op      [v1@(VInt _), v2@(VInt _)]
 | Just x       <- redNum2 op v1 v2
 =      return  $ Right x


-- World ------------------------------------------------------------
-- Get the value of a command-line argument.
step _ state PPArgument [VText name]
 = case lookup (Text.pack name) $ worldArguments $ stateWorld state of
        Just value     
         -> progress $ VText (Text.unpack value)

        Nothing 
         -> failure $ ErrorPrim $ ErrorArgumentUnknown (Text.pack name)


-- Tree IO  ---------------------------------------------------------
-- Load from the file system.
step _ _ PPLoad      [VText filePath]
 = case FilePath.takeExtension filePath of
        ".csv"  
         -> do  bs              <- BS8.readFile filePath
                let Right t     =  T.decodeCSV T.HasHeader bs        
                progress $ VTree t

        ".matryo"
         -> do  bs        <- BS.readFile filePath
                let result = Matryo.decodeTree filePath 
                           $ Text.decodeUtf8 bs

                case result of
                 Left  err      -> error $ show err
                 Right tree     -> progress $ VTree $ T.promiseTree tree
                                -- TODO: check the loaded tree.


        _ ->    failure  $ ErrorPrim $ ErrorStoreUnknownFileFormat filePath


-- Store to the file system.
step _ _ PPStore     [VText filePath, VTree tree]
 = case FilePath.takeExtension filePath of
        ".csv"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> BS8.hPutStr h (T.encodeCSV T.HasHeader tree)
                progress $ VUnit

        ".matryo"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> LText.hPutStr h (Matryo.prettyTree tree)
                progress $ VUnit

        ".tree"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> PP.hPutDoc h (T.ppTree mempty tree PP.<> PP.line)
                progress $ VUnit 

        _ ->    failure  $ ErrorPrim $ ErrorLoadUnknownFileFormat filePath


-- Tree Sampling ----------------------------------------------------
-- Take the initial n branches of each subtree.
step _ _ PPInitial [vn, VTree   tree]
 | Just n  <- takeVNat vn
 =      progress $ VTree   $ T.initial n tree

step _ _ PPInitial [vn, VForest forest]
 | Just n  <- takeVNat vn
 =      progress $ VForest $ T.initial n forest


-- Take the final n branches of each subtree.
step _ _ PPFinal   [vn, VTree   tree]
 | Just n  <- takeVNat vn
 =      progress $ VTree   $ T.final n tree

step _ _ PPFinal   [vn, VForest forest]
 | Just n  <- takeVNat vn
 =      progress $ VForest $ T.final n forest


-- Sample n intermediate branches of each subtree.
step _ _ PPSample  [vn, VTree   tree]
 | Just n  <- takeVNat vn
 =      progress $ VTree   $ T.sample n tree

step _ _ PPSample  [vn, VForest forest]
 | Just n  <- takeVNat vn
 =      progress $ VForest $ T.sample n forest


-- Tree Gather / Group ----------------------------------------------
-- Gather subtrees.
step _ _ PPGather    [VList _ names, VTree tree]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VTree $ T.gatherTree names' tree


-- Group by a given key.
step _ _ PPGroup   [VName name, VForest forest]
 =      progress $ VForest
                 $ T.promiseForest
                 $ T.groupForest (Text.unpack name) forest


-- Tree Flattening --------------------------------------------------
step _ _ PPFlatten      [VTree tree]
 = do   progress $ VTree $ T.flattenTree tree


-- Tree Rename Fields -----------------------------------------------
step _ _ PPRenameFields [VList _ names, VTree tree ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VTree
                 $ T.promiseTree
                 $ T.renameFields names' tree

step _ _ PPRenameFields [VList _ names, VForest forest ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VForest
                 $ T.promiseForest
                 $ T.renameFields names' forest


-- Tree Permute Fields -----------------------------------------------
step _ _ PPPermuteFields [ VList _ names, VTree tree ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VTree
                 $ T.promiseTree
                 $ T.permuteFields names' tree

step _ _ PPPermuteFields [ VList _ names, VForest forest ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VForest
                 $ T.promiseForest
                 $ T.permuteFields names' forest

-- Tree Printing ----------------------------------------------------
step _ _ PPPrint [VTree tree]
 = do   LText.hPutStr System.stdout (Matryo.prettyTree tree)
        progress $ VUnit

step _ _ PPPrint [VVPAF (PVAtom a)]
 = do   LText.hPutStr System.stdout $ pprAtom a
        putStr "\n"
        progress $ VUnit


-- Tree Traversal ---------------------------------------------------
-- | Apply a per-tree function to the trees at the given path.
step self state PPAt [VList _ names, thunk, VTree tree0]
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
step self state PPOn [VList _ names, thunk, VTree tree0]
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

step _ _ p args
 -- Form a thunk from a partially applied primitive.
 | length args < arityOfOp p
 = do   progress $ VPAP (PAF (PVOp p) args)

 -- Primitive application is ill-typed.
 | otherwise
 = error $ unlines
         [ "datum-tree: ill typed primitive application\n"
         , "op      = " ++ show p
         , "n-args  = " ++ show (length args)
         , "arity   = " ++ show (arityOfOp p)
         , "args    = " ++ Text.ppShow args]


-- Helpers.
progress thunk = return $ Right thunk
failure  err   = return $ Left  err


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
 $ case curryThunkPrim thunk (PVForest forest0) of
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
                        ControlPAP (PAF (PVForest t) [])
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
 $ case curryThunkPrim thunk (PVTree tree0) of
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
                        ControlPAP (PAF (PVTree t) [])
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


---------------------------------------------------------------------------------------------------
-- | Reduce a binary numeric primop.
redNum2 :: PrimOp -> Value -> Value -> Maybe Value
redNum2 op (VInt x1) (VInt x2)
 = case op of
        PPAdd   -> Just $ VInt (x1 +     x2)
        PPSub   -> Just $ VInt (x1 -     x2)
        PPMul   -> Just $ VInt (x1 *     x2)
        PPDiv   -> Just $ VInt (x1 `div` x2)
        _       -> Nothing

redNum2 _ _ _
 = Nothing


-- | Take the name from an expression, if there is one.
takeXName :: Exp -> Maybe T.Name
takeXName xx
 = case xx of
        XAnnot _ x      -> takeXName x
        XName n         -> Just (Text.unpack n)
        _               -> Nothing


-- | Take a natural number from a value, if there is one.
--   This works for VNat and VInt.
takeVNat :: Value -> Maybe Int
takeVNat vv
 = case vv of
        VNat n          -> Just n
        VInt n | n >= 0 -> Just n
        _               -> Nothing

