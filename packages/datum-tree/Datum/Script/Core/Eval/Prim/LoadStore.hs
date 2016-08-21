
module Datum.Script.Core.Eval.Prim.LoadStore
        (step_LoadStore)
where
import Datum.Script.Core.Eval.Prim.Base

import Datum.Data.Tree.Codec.Matryo.Decode              ()
import qualified Datum.Data.Tree.Codec                  as T
import qualified Datum.Data.Tree.Codec.Matryo.Encode    as Matryo
import qualified Datum.Data.Tree.Codec.Matryo.Decode    as Matryo
import qualified Datum.Data.Tree.Codec.SExp.Pretty      as T
import qualified Datum.Data.Tree.Operator.Cast          as T

import qualified System.FilePath                        as FilePath
import qualified System.IO                              as System

import qualified Text.PrettyPrint.Leijen                as PP

import qualified Data.ByteString.Lazy.Char8             as BS8
import qualified Data.ByteString                        as BS
import qualified Data.Text.Lazy.IO                      as LText
import qualified Data.Text.Encoding                     as Text


-- Load from the file system.
step_LoadStore _ _ PPLoad      [VText filePath]
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
step_LoadStore _ _ PPStore     [VText filePath, VTree tree]
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

step_LoadStore _ _ _ _
 =      crash

