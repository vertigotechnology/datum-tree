
module Datum.Script.Eval.Prim
        ( step
        , Error)
where
import Datum.Script.Eval.Error
import Datum.Script.Eval.State
import Datum.Script.Eval.Env                    (Thunk(..))
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


-- | Evaluate a primitive applied to some arguments.
step :: PrimOp -> [Thunk] -> IO (Either Error Thunk)

-- Numeric ------------------------------------------------
step op      [v1@(VInt _), v2@(VInt _)]
 | Just x       <- redNum2 op v1 v2
 =      return  $ Right x


-- Tree ---------------------------------------------------
-- Load from the file system.
step PPLoad      [VText filePath]
 = case FilePath.takeExtension filePath of
        ".csv"  
         -> do  bs              <- BS8.readFile filePath
                let Right t     =  T.decodeCSV T.HasHeader bs        
                return $ Right (VPrim (PVTree t) [])

        _ ->    return $ Left $ ErrorPrim $ ErrorStoreUnknownFileFormat filePath


-- Store to the file system.
step PPStore     [VText filePath, VTree tree]
 = case FilePath.takeExtension filePath of
        ".tree"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> PP.hPutDoc h (T.ppTree mempty tree PP.<> PP.line)

                return $ Right (VPrim (PVAtom AUnit) [])

        _ ->    return $ Left $ ErrorPrim $ ErrorLoadUnknownFileFormat filePath


-- Take the initial n branches of each subtree.
step PPInitial   [VNat n, VTree tree]
 =      return  $ Right $ VTree $ T.initial n tree


-- Take the final n branches of each subtree.
step PPFinal     [VNat n, VTree tree]
 =      return  $ Right $ VTree $ T.final n tree


-- Sample n intermediate branches of each subtree.
step PPSample    [VInt n, VTree tree]
 =      return  $ Right $ VTree $ T.sample n tree


-- Gather subtrees.
step PPGather    [VTreePath treePath, VTree tree]
 = do   let path' = map Text.unpack treePath
        return  $ Right $ VTree $ T.gatherTree path' tree

        
-- Group by a given key.
-- TODO: need a way to specify the correct level.
step PPGroup   [VName name, VTree tree]
 =      return  $ Right $ VTree 
                $ T.promiseTree
                $ T.mapForests (T.groupForest $ Text.unpack name) tree

-- TODO: need a way to specify the correct level.
step PPRenameFields [ VList XTName names, VTree tree ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        return  $ Right $ VTree
                $ T.promiseTree
                $ T.mapTrees (T.renameFields names') tree

step p args
 = do   return  $ Right (VPrim (PVOp p) args)



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
 

