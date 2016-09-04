
module Datum.Script.Kernel.Exp.Cast where

        
-- | Type casts.
data Cast
        -- | Run an effectful computation.
        = CastRun

        -- | Box up an effectful expression.
        | CastBox

deriving instance Show Cast
deriving instance Eq   Cast

