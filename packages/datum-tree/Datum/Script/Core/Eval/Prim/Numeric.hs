
module Datum.Script.Core.Eval.Prim.Numeric
        (step_Numeric)
where
import Datum.Script.Core.Eval.Prim.Base


step_Numeric _ _ op     [v1@(VInt _),     v2@(VInt _)]
 | Just x       <- redNum2 op v1 v2
 =      return  $ Right x


step_Numeric _ _ op     [v1@(VDecimal _), v2@(VDecimal _)]
 | Just x       <- redNum2 op v1 v2
 =      return  $ Right x


step_Numeric _ _ op     [v1@(VDecimal _), v2@(VInt _)]
 | Just x       <- redNum2 op v1 v2
 =      return  $ Right x


step_Numeric _ _ op     [v1@(VInt _),     v2@(VDecimal _)]
 | Just x       <- redNum2 op v1 v2
 =      return  $ Right x


step_Numeric _ state _ _
 =      crash state


-- | Reduce a binary numeric primop.
redNum2 :: PrimOp -> Value -> Value -> Maybe Value

redNum2 op (VInt x1)     (VInt x2)
 = case op of
        PPAdd   -> Just $ VInt (x1 +     x2)
        PPSub   -> Just $ VInt (x1 -     x2)
        PPMul   -> Just $ VInt (x1 *     x2)
        PPDiv   -> Just $ VInt (x1 `div` x2)
        _       -> Nothing

redNum2 op (VDecimal x1) (VDecimal x2)
 = case op of
        PPAdd   -> Just $ VDecimal (x1 +  x2)
        PPSub   -> Just $ VDecimal (x1 -  x2)
        PPMul   -> Just $ VDecimal (x1 *  x2)
        PPDiv   -> Just $ VDecimal (x1 /  x2)
        _       -> Nothing

redNum2 op (VDecimal x1) (VInt x2)
 = case op of
        PPAdd   -> Just $ VDecimal (x1 +     (fromIntegral x2))
        PPSub   -> Just $ VDecimal (x1 -     (fromIntegral x2))
        PPMul   -> Just $ VDecimal (x1 *     (fromIntegral x2))
        PPDiv   -> Just $ VDecimal (x1 /     (fromIntegral x2))
        _       -> Nothing

redNum2 op (VInt x1)     (VDecimal x2)
 = case op of
        PPAdd   -> Just $ VDecimal ((fromIntegral x1) + x2)
        PPSub   -> Just $ VDecimal ((fromIntegral x1) - x2)
        PPMul   -> Just $ VDecimal ((fromIntegral x1) * x2)
        PPDiv   -> Just $ VDecimal ((fromIntegral x1) / x2)
        _       -> Nothing

redNum2 _ _ _
 = Nothing
