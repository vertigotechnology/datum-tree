
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
        -- Load a CSV file as a tree.
        ".csv"  
         -> do  bs              <- BS8.readFile filePath
                let Right t     =  T.decodeCSV T.HasHeader bs        
                progress $ VTree t

        -- Load a Matryo file as a tree.
        ".matryo"
         -> do  bs        <- BS.readFile filePath
                let result = Matryo.decodeTree filePath 
                           $ Text.decodeUtf8 bs

                case result of
                 Left  err      -> error $ show err
                 Right tree     -> progress $ VTree $ T.promiseTree tree
                                -- TODO: check the loaded tree.

        -- We don't recognise the extension on the provided file path.
        _ ->    failure  $ ErrorPrim $ ErrorStoreUnknownFileFormat filePath


-- Store to the file system.
step_LoadStore _ _ PPStore     [VText filePath, VTree tree]
 = case FilePath.takeExtension filePath of
        -- Store tree as a CSV file.
        ".csv"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> BS8.hPutStr h (T.encodeCSV T.HasHeader tree)
                progress $ VUnit

        -- Store tree as a Matryo file.
        ".matryo"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> LText.hPutStr h (Matryo.prettyTree tree)
                progress $ VUnit

        -- Store tree as a SExp tree file.
        ".tree"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> PP.hPutDoc h (T.ppTree mempty tree PP.<> PP.line)
                progress $ VUnit 

        -- We don't recognise the extension on the provided file path.
        _ ->    failure  $ ErrorPrim $ ErrorLoadUnknownFileFormat filePath


-- The current term is ill-typed.
step_LoadStore _ _ _ _
 =      crash

