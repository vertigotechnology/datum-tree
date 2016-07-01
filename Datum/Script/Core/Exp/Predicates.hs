
module Datum.Script.Core.Exp.Predicates
--        (isNormalOpenX)
where
{-
import Datum.Script.Core.Exp.Generic
import Datum.Script.Core.Exp.Compounds


-- | Check whether the given expression is in normal form.
--
--   The expression is open meaning it has variables that have not had
--   values substituted into them. If we find a variable in a head position
--   then we say the expression is not in normal form.
--
isNormalOpenX 
        :: (GXPrim l -> Int)    -- ^ Determine the arity of a primitive operator.
        -> GExp l               -- ^ Expression to check.
        -> Bool

isNormalOpenX arityOfPrim xx
 = case xx of
        XAnnot _ x      -> isNormalOpenX arityOfPrim x
        XPrim{}         -> True
        XVar{}          -> False
        XCast  _ x      -> isNormalOpenX arityOfPrim x
        XAbs{}          -> True

        XApp x1 x2
         -> let (xf, xs)        = takeXApps' x1 x2
            in  case stripXAnnot xf of
                 XAnnot{}       -> False
                 XPrim p        -> length xs >= arityOfPrim p
                 XVar{}         -> False
                 XCast{}        -> False
                 XAbs{}         -> False
                 XApp{}         -> False

        XLet
-}