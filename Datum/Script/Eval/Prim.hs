
module Datum.Script.Eval.Prim
        ( step
        , Error)
where
import Datum.Script.Eval.Error
import Datum.Script.Eval.State
import Datum.Script.Eval.Env                    (Thunk(..), PAP(..))
import Datum.Script.Core.Exp
import qualified Datum.Data.Tree                as T
import qualified Datum.Data.Tree.Codec          as T
import qualified Datum.Data.Tree.SExp.Pretty    as T
import qualified Datum.Data.Tree.Operator.Cast  as T
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified System.FilePath                as FilePath
import qualified System.IO                      as System
import qualified Text.PrettyPrint.Leijen        as PP
import qualified Data.Text                      as Text
import qualified System.IO.Unsafe               as System
import qualified Text.Show.Pretty               as Text
import qualified Datum.Script.Eval.Env          as Env

progress thunk = return $ Right thunk
failure  err   = return $ Left  err

---------------------------------------------------------------------
-- | Evaluate a primitive applied to some arguments.
step    :: (State -> IO (Either Error (Maybe State)))
        -> State
        -> PrimOp -> [Thunk] -> IO (Either Error Thunk)


-- Numeric ----------------------------------------------------------
step _ _ op      [v1@(VInt _), v2@(VInt _)]
 | Just x       <- redNum2 op v1 v2
 =      return  $ Right x


-- Tree IO  ---------------------------------------------------------
-- Load from the file system.
step _ _ PPLoad      [VText filePath]
 = case FilePath.takeExtension filePath of
        ".csv"  
         -> do  bs              <- BS8.readFile filePath
                let Right t     =  T.decodeCSV T.HasHeader bs        
                progress $ VTree t

        _ ->    failure  $ ErrorPrim $ ErrorStoreUnknownFileFormat filePath


-- Store to the file system.
step _ _ PPStore     [VText filePath, VTree tree]
 = case FilePath.takeExtension filePath of
        ".tree"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> PP.hPutDoc h (T.ppTree mempty tree PP.<> PP.line)
                progress $ VUnit 

        _ ->    failure  $ ErrorPrim $ ErrorLoadUnknownFileFormat filePath


-- Tree Sampling ----------------------------------------------------
-- Take the initial n branches of each subtree.
step _ _ PPInitial   [VNat n, VTree tree]
 =      progress $ VTree $ T.initial n tree


-- Take the final n branches of each subtree.
step _ _ PPFinal     [VNat n, VTree tree]
 =      progress $ VTree $ T.final n tree


-- Sample n intermediate branches of each subtree.
step _ _ PPSample    [VInt n, VTree tree]
 =      progress $ VTree $ T.sample n tree


-- Tree Gather / Group ----------------------------------------------
-- Gather subtrees.
step _ _ PPGather    [VTreePath treePath, VTree tree]
 = do   let path' = map Text.unpack treePath
        progress $ VTree $ T.gatherTree path' tree


-- Group by a given key.
step _ _ PPGroup   [VName name, VForest forest]
 =      progress $ VForest
                 $ T.promiseForest
                 $ T.groupForest (Text.unpack name) forest

-- Tree Renaming ----------------------------------------------------
step _ _ PPRenameFields [ VList _ names, VForest forest ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VForest
                 $ T.promiseForest
                 $ T.renameFields names' forest


-- Tree Traversal ---------------------------------------------------
-- | Apply a per-tree function to the trees at the given path.
step self state PPAt [VList _ names, thunk, VTree tree0]
  = do  let Just names' 
                = sequence
                $ map takeXName names

        let inception :: T.Path -> T.Tree 'T.O -> T.Tree 'T.O
            inception path tree
                = T.promiseTree
                $ System.unsafePerformIO
                $ liftTreeTransformIO 
                        self 
                        thunk 
                        state { stateContext = [] }
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

        let inception :: T.Path -> T.Forest 'T.O -> T.Forest 'T.O
            inception path forest
                = T.promiseForest
                $ System.unsafePerformIO
                $ liftForestTransformIO 
                        self thunk 
                        state { stateContext = [] }
                        path forest

        progress $ VTree
                 $ T.promiseTree
                 $ T.mapForestOfTreeOn
                        names' inception
                        mempty tree0


step _ _ p args
 -- Form a thunk from a partially applied primitive.
 | length args < arityOfOp p
 = do   progress $ VPAP (PAP (PVOp p) args)

 -- Primitive application is ill-typed.
 | otherwise
 = error $ unlines
         [ "datum-tree: ill typed primitive application\n"
         , "op      = " ++ show p
         , "n-args  = " ++ show (length args)
         , "arity   = " ++ show (arityOfOp p)
         , "args    = " ++ Text.ppShow args]


----------------------------------------------------------------------------------------------------
redNum2 :: PrimOp -> Thunk -> Thunk -> Maybe Thunk
redNum2 op (VInt x1) (VInt x2)
 = case op of
        PPAdd   -> Just $ VInt (x1 +     x2)
        PPSub   -> Just $ VInt (x1 -     x2)
        PPMul   -> Just $ VInt (x1 *     x2)
        PPDiv   -> Just $ VInt (x1 `div` x2)
        _       -> Nothing

redNum2 _ _ _
 = Nothing


takeXName :: Exp -> Maybe T.Name
takeXName xx
 = case xx of
        XAnnot _ x      -> takeXName x
        XName n         -> Just (Text.unpack n)
        _               -> Nothing


-------------------------------------------------------------------------------

liftForestTransformIO
        :: (State -> IO (Either Error (Maybe State)))
                        -- ^ Stepper function for general expressions.
        -> Thunk        -- ^ Expression mapping trees to trees.
        -> State        -- ^ Starting state.
        -> T.Path
        -> T.Forest 'T.O
        -> IO (T.Forest 'T.O)

liftForestTransformIO sstep thunk state0 _path0 forest0
 = orivour
 $ case curryThunkPrim thunk (PVForest forest0) of
        VClosure x1 env 
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
                        ControlPAP (PAP (PVForest t) [])
                                -> return t
                        focus   -> error $ "datum-tree: wrong type in internal forest evaluation "
                                ++ Text.ppShow focus


-------------------------------------------------------------------------------
liftTreeTransformIO
        :: (State -> IO (Either Error (Maybe State)))
                        -- ^ Stepper function for general expressions.
        -> Thunk        -- ^ Expression mapping trees to trees.
        -> State        -- ^ Starting state.
        -> T.Path
        -> T.Tree 'T.O
        -> IO (T.Tree 'T.O)

liftTreeTransformIO sstep thunk state0 _path0 tree0
 = orivour
 $ case curryThunkPrim thunk (PVTree tree0) of
        VClosure x1 env 
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
                        ControlPAP (PAP (PVTree t) [])
                                -> return t
                        focus   -> error 
                                $  "datum-tree: wrong type in internal evaluation "
                                ++ Text.ppShow focus


curryThunkPrim  :: Thunk -> Prim -> Thunk
curryThunkPrim tt pArg
 = case tt of
        VClosure x1 env
         -> VClosure (XApp x1 (XPrim pArg)) env

        VPAP (PAP p ts)
         -> VPAP (PAP p (ts ++ [VPAP (PAP pArg [])]))

