
module Datum.Script.Eval.Prim
        ( step
        , Error)
where
import Datum.Script.Eval.Error
import Datum.Script.Eval.State
import Datum.Script.Eval.Env                    (Value(..))
import Datum.Script.Exp.Core
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
step :: Prim -> [Value] -> IO (Either Error Value)

-- Load from the file system.
step PVLoad      [VFilePath filePath]
 = case FilePath.takeExtension filePath of
        ".csv"  
         -> do  bs              <- BS8.readFile filePath
                let Right t     =  T.decodeCSV T.HasHeader bs        
                return $ Right (VPrim (PVTree t) [])

        _ ->    return $ Left  (ErrorPrim (ErrorStoreUnknownFileFormat filePath))


-- Store to the file system.
step PVStore     [VFilePath filePath, VTree tree]
 = case FilePath.takeExtension filePath of
        ".tree"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> PP.hPutDoc h (T.ppTree mempty tree PP.<> PP.line)

                return $ Right (VPrim (PVAtom AUnit) [])

        _ ->    return $ Left $ ErrorPrim $ ErrorLoadUnknownFileFormat filePath


-- Gather subtrees.
step PVGather    [VTreePath treePath, VTree tree]
 = do   let path' = map Text.unpack treePath
        let tree' = T.gatherTree path' tree
        return  $ Right (VVPrim (PVTree tree'))


-- Rename fields in a tree.
step PVRenameFields [ VList XTName names, VTree tree ]
 = do   let names' = [ Text.unpack n | XName n <- names]

        -- PROMISE: Renaming fields renames the meta-data, 
        --          so the result is well typed.
        let tree'  = T.promiseTree
                   $ T.mapTrees (T.renameFields names') tree

        return  $ Right (VVPrim (PVTree tree'))

step p args
 = do   return  $ Right (VPrim p args)



