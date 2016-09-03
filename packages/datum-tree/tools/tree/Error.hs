
module Error where
import Config                                   (Config)
import Text.PrettyPrint.Leijen
import qualified Datum.Script.Core.Eval.Error   as Eval
import qualified Datum.Script.Core.Eval         as Eval
import qualified System.IO                      as System


-- | Report a runtime error to the console,
--   and dump debugging information if requested.
reportRuntimeError 
        :: Config
        -> Eval.State
        -> Eval.Error
        -> IO ()

reportRuntimeError _config _state err
 = do
        hPutDoc System.stderr 
         $ Eval.ppError err <> line

