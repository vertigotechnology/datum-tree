
module Datum.Script.Core.Eval
        ( State   (..)
        , World   (..)
        , Control (..)
        , Context (..)
        , stateInit

        , Error (..)
        , step)
where
import Datum.Script.Core.Eval.State
import Datum.Script.Core.Eval.Error
import Datum.Script.Core.Eval.Env               (Env)
import Datum.Script.Core.Eval.Value
import Datum.Script.Core.Exp
import qualified Datum.Script.Core.Eval.Env     as Env
import qualified Datum.Script.Core.Eval.Prim    as Prim
import qualified Data.Set                       as Set
import qualified Data.List                      as List


-- | Perform a single step evaluation of the expression.
step :: State -> IO (Either Error (Maybe State))

step   state@(State world env ctx ctl)
 = let  failure  err = return $ Left err
        progress ss  = return $ Right $ Just ss
        done         = return $ Right $ Nothing

   in case ctl of
        -- Silently look through annotations.
        ControlExp (XAnnot _ x)
         ->     step $ State world env ctx $ ControlExp x


        -- Silently promote prims to PAPs.
        ControlExp (XPrim p)
         ->     step $ State world env ctx $ ControlPAP (PAP p [])


        -- Other fragment specific prims are fully evaluated.
        ControlExp (XFrag p)
         ->     step $ State world env ctx $ ControlPAP (PAF p [])


        -- Silently look through casts.
        ControlExp (XCast _ x)
         ->     step $ State world env ctx $ ControlExp x


        -- Lookup value of variable from the environment.
        ControlExp (XVar u)
         -> case Env.lookup u env of
                Just (VClo (Clo x' env'))
                 -> progress $ State world env' ctx 
                             $ ControlExp x'

                Just (VPAP pp)
                 -> progress $ State world Env.empty ctx 
                             $ ControlPAP pp

                Nothing
                 -> failure  $ ErrorCore $ ErrorCoreUnboundVariable u


        -- Stash the argument expression in the context and
        -- begin evaluating the functional expression.
        ControlExp (XApp x1 x2)
         -> do  let ctx'  = ContextAppArg (VClo (Clo x2 env)) ctx
                progress $ State world env ctx' 
                         $ ControlExp x1


        -- Add recursive let-bindings to the environment.
        --   While we're here we trim them to only the ones that are
        --   actually needed free in the body.
        ControlExp (XRec bxs x2)
         -> do  
                -- Build thunks for each of the bindings.
                let (bs, xs) = unzip bxs
                let ts    = map (\x -> trimValue $ VClo (Clo x env)) xs

                -- Make a new environment containing the thunks,
                -- trimmed to just those that are needed in the body.
                -- Trim enviroment to just 
                let fvs   = freeVarsX Set.empty x2
                let keep uu 
                     = case uu of
                        UName n -> Set.member n fvs
                        _       -> True

                let env'  = Env.trim keep
                          $ List.foldl' (\e (b, t) -> Env.insert b t e) Env.empty
                          $ zip bs ts

                -- Add new environment to existing one.
                let env'' = Env.union env env'

                progress $ State world env'' ctx 
                         $ ControlExp x2


        -- Evaluate fully applied primitive applications in PAP form.
        ControlPAP (PAF (PVOp op) args)
         | length args == arityOfPrimOp op
         -> do  result  <- Prim.step step state op args
                case result of
                 Left err 
                  -> failure err

                 Right (VClo (Clo x' env'))
                  -> progress $ State world env' ctx      $ ControlExp x'

                 Right (VPAP pap)
                  -> progress $ State world Env.empty ctx $ ControlPAP pap


        _ -> case ctx of
                -- When the context is empty we should have ended up with
                -- a value. If not then our evaluation rules are borked.
                ContextNil 
                 -> case ctl of
                        ControlExp XAbs{} -> done
                        ControlPAP{}      -> done
                        _                 -> failure $ ErrorCore ErrorCoreStuck


                -- Finished evaluating the functional expression, 
                -- so stash that in the context and evaluate the argument.
                ContextAppArg (VClo (Clo xArg envArg)) ctx'
                 -> do  let thunk = trimValue $ thunkify ctl env
                        let ctx'' = ContextAppFun thunk ctx'
                        progress $ State world envArg ctx'' $ ControlExp xArg


                -- Finished evaluating the argument, 
                -- so grab the function from the context and apply it.
                ContextAppFun (VClo (Clo (XAbs b _t xBody) envFun)) ctx'
                 -> do  let thunk = trimValue $ thunkify ctl env
                        let env'  = Env.insert b thunk envFun
                        progress $ State world env' ctx' $ ControlExp xBody


                -- Application of a primitive where its next argument
                -- comes from the context.
                ContextAppFun thunk ctx'
                 | Just (PVOp op, ts) 
                   <- case thunk of
                        VClo (Clo (XFrag p) _env)  -> Just (p, [])
                        VPAP (PAF p ts)            -> Just (p, ts)
                        _                          -> Nothing

                 -> do  let t'     = trimValue $ thunkify ctl env
                        let state' = state { stateContext = ctx' }
                        result <- Prim.step step state' op (ts ++ [t'])
                        case result of
                         Left  err    -> failure err

                         Right (VClo (Clo x' env'))
                          -> progress $ State world env' ctx'      $ ControlExp x'

                         Right (VPAP pap)
                          -> progress $ State world Env.empty ctx' $ ControlPAP pap


                -- The context is not empty but we can't make progress.
                -- This is probably cause by a type error in the user program.
                _ -> failure $ ErrorCore $ ErrorCoreType state


-- | Pack a control with the current environment into a thunk.
thunkify :: Control -> Env -> Value
thunkify ctl env
 = case ctl of
        ControlExp xx  -> VClo (Clo xx env)
        ControlPAP pap -> VPAP pap

