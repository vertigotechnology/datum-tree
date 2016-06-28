
module Datum.Script.Core.Exp.Cast where

        
-- | Type casts.
data Cast
        -- | Run an effectful computation.
        = CRun

        -- | Box up an effectful expression.
        | CBox

deriving instance Show Cast
deriving instance Eq   Cast

