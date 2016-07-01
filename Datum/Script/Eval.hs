
module Datum.Script.Eval
        ( Frame (..)
        , State (..)
        , stateInit

        , Error (..)
        , step)
where
import Datum.Script.Eval.State
import Datum.Script.Eval.Error
import Datum.Script.Eval.Env            (Thunk(..), PAP (..))
import Datum.Script.Core.Exp
import qualified Datum.Script.Eval.Env  as Env
import qualified Data.Set               as Set
import qualified Datum.Script.Eval.Prim as Prim
import Text.Show.Pretty
import qualified Data.List              as List


-- | Perform a single step evaluation of the expression.
step :: State -> IO (Either Error (Maybe State))

step   state@(State env ctx ctl)
 = let  failure  err = return $ Left err
        progress ss  = return $ Right $ Just ss
        done         = return $ Right $ Nothing

   in case ctl of
        -- Silently look through annotations.
        Left (XAnnot _ x)
         ->     step $ State env ctx  (Left x)

        -- Silently promote prims to paps
        Left (XPrim p)
         ->     step $ State env ctx (Right $ PAP p [])

        -- Silently look through casts.
        Left (XCast _ x)
         ->     step $ State env ctx (Left x)

        -- Lookup value of variable from the environment.
        Left (XVar u)
         -> case Env.lookup u env of
                Just (VClosure x' env')
                 -> progress $ State env' ctx (Left x')

                Just (VPAP (PAP p ts))
                 -> progress $ State Env.empty ctx (Right $ PAP p ts)

                Nothing
                 -> failure  $ ErrorCore $ ErrorCoreUnboundVariable u


        -- Stash the argument expression in the context and
        -- begin evaluating the functional expression.
        Left (XApp x1 x2)
         -> do  let ctx'  = FrameAppArg (VClosure x2 env) : ctx
                progress $ State env ctx' (Left x1)


        -- Add recursive let-bindings to the environment.
        --   While we're here we trim them to only the ones that are
        --   actually needed free in the body.
        Left (XRec bxs x2)
         -> do  
                -- Build thunks for each of the bindings.
                let (bs, xs) = unzip bxs
                let ts    = map (\x -> Env.trimThunk $ VClosure x env) xs

                -- Make a new environment containing the thunks,
                -- trimmed to just those that are needed in the body.
                -- Trim enviroment to just 
                let fvs   = Env.freeVarsX Set.empty x2
                let keep uu 
                     = case uu of
                        UName n -> Set.member n fvs
                        _       -> True

                let env'  = Env.trim keep
                          $ List.foldl' (\e (b, t) -> Env.insert b t e) Env.empty
                          $ zip bs ts

                -- Add new environment to existing one.
                let env'' = Env.union env env'

                progress $ State env'' ctx (Left x2)

        Right (PAP (PVOp op) args)
         | length args == arityOfOp op
         -> do  result  <- Prim.step step state op args
                case result of
                 Left err -> error $ show err
                 Right (VClosure x' env') 
                  -> progress $ State env' ctx (Left x')

                 Right (VPAP pap)
                  -> progress $ State Env.empty ctx (Right pap)


        _ -> case ctx of
                -- Finished evaluating the functional expression, 
                -- so stash that in the context and evaluate the argument.
                FrameAppArg (VClosure xArg envArg) : ctx'
                 -> do  let thunk = Env.trimThunk $ thunkify ctl env
                        let ctx'' = FrameAppFun thunk : ctx'
                        progress $ State envArg ctx'' (Left xArg)

                -- Finished evaluating the argument, 
                -- so grab the function from the context and apply it.
                FrameAppFun (VClosure (XAbs b _t xBody) envFun) : ctx'
                 -> do  let thunk = Env.trimThunk $ thunkify ctl env
                        let env'  = Env.insert b thunk envFun
                        progress $ State env' ctx'(Left xBody)

                FrameAppFun thunk : ctx'
                 | Just (PVOp op, ts) 
                   <- case thunk of
                        VClosure (XPrim p) _envFun -> Just (p, [])
                        VPAP (PAP p ts)            -> Just (p, ts)
                        _                          -> Nothing

                 -> do  let t'     = Env.trimThunk $ thunkify ctl env
                        let state' = state { stateContext = ctx' }
                        result <- Prim.step step state' op (ts ++ [t'])

                        case result of
                         Left  err    -> error    $ show err

                         Right (VClosure x' env')
                          -> progress $ State env' ctx' (Left x')

                         Right (VPAP pap)
                          -> progress $ State Env.empty ctx' (Right pap)

                [] -> case ctl of
                        Left XAbs{}  -> done
                        Right{}      -> done
                        _            -> error $ "invalid state " ++ ppShow state

                _ -> error $ "invalid state " ++ ppShow state


thunkify ctl env
 = case ctl of
        Left xx          -> VClosure xx env
        Right (PAP p ts) -> VPAP (PAP p ts)
