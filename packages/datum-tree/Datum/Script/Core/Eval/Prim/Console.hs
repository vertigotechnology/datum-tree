
module Datum.Script.Core.Eval.Prim.Console
        (step_Console)
where
import Datum.Script.Core.Eval.Prim.Base
import Datum.Script.Core.Eval.Pretty.Build
import qualified System.IO                              as System
import qualified Datum.Data.Tree.Codec.Matryo.Encode    as Matryo
import qualified Data.Text.Lazy.IO                      as LText
import qualified Data.Text.Lazy.Builder                 as LText

step_Console _ _ PPPrint [VTree tree]
 = do   LText.hPutStr System.stdout (Matryo.prettyTree tree)
        progress $ VUnit

step_Console _ _ PPPrint [VVPAF (PVData d)]
 = do   LText.hPutStrLn System.stdout 
         $ LText.toLazyText
         $ buildPrimData d


        progress $ VUnit

step_Console _ _ PPPrint [VVPAP PVUnit]
 = do   putStr "()\n"
        progress $ VUnit

step_Console _ state _ _
 =      crash state