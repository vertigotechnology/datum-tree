
module Main where
import Config
import Load
import Data.Default
import Text.Show.Pretty
import Control.Monad
import qualified Datum.Script.Core.Exp          as Exp
import qualified Datum.Script.Eval              as Eval
import qualified Datum.Script.Eval.Pretty       as Eval
import qualified Datum.Script.Eval.Env          as Eval
import qualified System.Environment             as System
import qualified System.Exit                    as System
import qualified Data.Text.Lazy.IO              as LText


main
 = do   args    <- System.getArgs 
        config  <- parseArgs args def

        case configFile config of
         Just filePath  -> runScript config filePath
         _              -> do   putStrLn usage
                                System.exitSuccess


-- | Run the script in the given file.
runScript :: Config -> FilePath -> IO ()
runScript config filePath
 = do   -- Read the client module.
        strSource       <- readFile filePath

        -- Parse the client module text and convert to the core language.
        xCore           <- loadToCore (configDump config) filePath strSource

        -- Create a new machine state to evaluate the core expression.
        let world       = Eval.World 
                        { Eval.worldArguments   = configArguments config }

        let state       = Eval.stateInit world xCore

        -- Evaluate the script.
        state'         <- eval config state


        case Eval.stateControl state' of
         -- Suppress printing of unit value.
         -- Many scripts store their results to some destination file, 
         -- and we don't need the '()' from the final store operation
         -- printed to the console. The GHCi console also does this.
         Eval.ControlPAP (Eval.PAP (Exp.PVAtom Exp.AUnit) [])
          |  not $ configShowUnit config
          -> return ()

         Eval.ControlExp (Exp.XPrim (Exp.PVAtom Exp.AUnit))
          |  not $ configShowUnit config
          -> return ()

         -- Evaluation has produced some non-unit result, 
         -- so print it to the console.
         _ -> do let ppConfig = Eval.Config
                              { Eval.configTreeFormat = Eval.TreeFormatMatryo }
                 LText.putStrLn (Eval.pprControl ppConfig $ Eval.stateControl state')
                 return ()


-- | Eval loop for a script state.
eval :: Config -> Eval.State -> IO Eval.State
eval config state
 = do   
        when (configTrace config)
         $ putStrLn $ ppShow state

        result  <- Eval.step state
        case result of 
         Left  err              -> error $ show err
         Right Nothing          -> return state
         Right (Just state')    -> eval config state'

