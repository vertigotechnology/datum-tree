
module Datum.Script.Eval
        ( Frame (..)
        , State (..)
        , isDone
        , stateInit

        , Error (..)
        , step)
where
import Datum.Script.Eval.State
import Datum.Script.Eval.Error
import Datum.Script.Eval.Env            (Value(..))
import Datum.Script.Core.Exp
import qualified Datum.Script.Eval.Env  as Env
import qualified Datum.Script.Eval.Prim as Prim


-- | Perform a single step evaluation of the expression.
step :: State -> IO (Either Error State)

step   (State env ctx (Left xx))
 = case xx of
        -- Look through annotations.
        XAnnot _ x
         ->     return  $ Right $ State env ctx (Left x)

        -- Lookup value of variable from the environment.
        XVar u
         -> let Just v' = Env.lookup u env
            in  return  $ Right $ State env ctx (Right v')

        -- Primitives are already values.
        XPrim p
         -> case ctx of
                FrameAppLeft x2 : ctx'
                 -> let ctx2    = FrameAppRight (VPrim p []) : ctx'
                    in  return  $ Right $ State env ctx2 (Left x2)

                _ ->    return  $ Right $ State env ctx  (Right (VPrim p []))

        -- Look through type casts.
        XCast _ x
         ->     return  $ Right $ State env ctx (Left x)

        -- Abstractions are already values.
        XAbs{}
         ->     return  $ Right $ State env ctx (Right (VClosure xx env))

        -- Evaluate application.
        XApp x1 x2
         -> let ctx'    = FrameAppLeft x2 : ctx
            in  return  $ Right $ State env ctx' (Left x1)

step (State env ctx  (Right vv))
 = case (ctx, vv) of

        -- We have reduced the left of an application to a value,
        -- now reduce the argument.
        (FrameAppLeft x2 : ctx', v1)
         -> case v1 of
                VClosure (XAbs{}) _
                 -> let ctx2    = FrameAppRight v1 : ctx'
                    in  return  $ Right $ State env ctx2 (Left x2)

                VPrim{}
                 -> let ctx2    = FrameAppRight v1 : ctx'
                    in  return  $ Right $ State env ctx2 (Left x2)

                _ -> return $ Left $ Error "invalid state"

        -- We have reduced the right of an application to a value,
        -- so perform the application.
        (FrameAppRight v1 : ctx', v2)
         -> case v1 of
                -- Application of an abstraction to an argument.
                --   Build a closure that binds the formal parameter to the argument.
                VClosure (XAbs b _t x11) env'
                 ->  return $ Right $ State env ctx' 
                            $ Right (VClosure x11 (Env.insert b v2 env'))

                -- Application of a primitive operator to some arguments.
                --   The primitive may or may not be partially applied.
                VPrim (PVOp p) args
                 -> do  result <- Prim.step p (args ++ [v2])
                        case result of
                         Left err -> return $ Left err
                         Right v' -> return $ Right $ State env ctx' $ Right v'

                _ -> return $ Left $ Error "invalid state"

        _ -> return $ Left $ Error "invalid state"

