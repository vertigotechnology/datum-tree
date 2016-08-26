
module Datum.Script.Core.Eval.Prim.LoadStore
        (step_LoadStore)
where
import Datum.Script.Core.Eval.Prim.Base

import qualified Datum.Data.Tree.Check                  as Tree
import qualified Datum.Data.Tree.Codec.XSV              as Tree
import qualified Datum.Data.Tree.Codec.SExp.Pretty      as Tree
import qualified Datum.Data.Tree.Codec.Matryo.Encode    as Matryo
import qualified Datum.Data.Tree.Codec.Matryo.Decode    as Matryo

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
        -- Load a CSV (Comma Separated Values) file as a tree.
        ".csv"  
         -> do  bs              <- BS8.readFile filePath
                let Right t     =  Tree.decodeCSV Tree.HasHeader bs
                progress $ VTree t

        -- Load a TSV (Tab Separated Values) file as a tree.
        ".tsv"  
         -> do  bs              <- BS8.readFile filePath
                let Right t     =  Tree.decodeTSV Tree.HasHeader bs
                progress $ VTree t

        -- Load a Matryo file as a tree.
        ".matryo"
         -> do
                -- Read file and parse the tree.
                bs        <- BS.readFile filePath
                let result = Matryo.decodeTree filePath $ Text.decodeUtf8 bs

                case result of
                 -- Parsing failed.
                 Left  err
                  -> failure $ ErrorPrim $ ErrorLoadParseError filePath err

                 -- Parsing succeeded, but we still need to type check it.
                 Right tree
                  -> case Tree.check tree of
                        -- Type checking failed.
                        Left err
                         -> failure  $ ErrorPrim $ ErrorLoadTypeError filePath err

                        -- Type checking suceeded, so we have a good tree.
                        Right tree_checked
                         -> progress $ VTree tree_checked


        -- We don't recognise the extension on the provided file path.
        _ ->    failure  $ ErrorPrim $ ErrorStoreUnknownFileFormat filePath


-- Store to the file system.
step_LoadStore _ _ PPStore     [VText filePath, VTree tree]
 = case FilePath.takeExtension filePath of
        -- Store a tree as a CSV (Comma Separated Values) file.
        ".csv"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> BS8.hPutStr   h $ Tree.encodeCSV Tree.HasHeader tree
                progress $ VUnit

        -- Store a tree as a TSV (Tab Separated Values) file.
        ".tsv"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> BS8.hPutStr   h $ Tree.encodeTSV Tree.HasHeader tree
                progress $ VUnit

        -- Store tree as a Matryo file.
        ".matryo"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> LText.hPutStr h $ Matryo.prettyTree tree
                progress $ VUnit

        -- Store tree as a SExp tree file.
        ".tree"
         -> do  System.withFile filePath System.WriteMode
                 $ \h -> PP.hPutDoc h (Tree.ppTree mempty tree PP.<> PP.line)
                progress $ VUnit 

        -- We don't recognise the extension on the provided file path.
        _ ->    failure  $ ErrorPrim $ ErrorLoadUnknownFileFormat filePath


-- The current term is ill-typed.
step_LoadStore _ _ _ _
 =      crash

