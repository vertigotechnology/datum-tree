
module Datum.Script.Source.Exp.Compounds
        ( -- * Annotations
          stripXAnnot

          -- * Applications
        , makeXApps
        , takeXApps)
where
import Datum.Script.Source.Exp.Generic


-------------------------------------------------------------------------------
-- | Strip annotations from a source expression.
stripXAnnot :: GExp l -> GExp l
stripXAnnot xx
 = case xx of
        XAnnot _ x      -> stripXAnnot x
        XPrim{}         -> xx
        XVar{}          -> xx
        XCast  c x      -> XCast c   (stripXAnnot x)
        XAbs   b t x    -> XAbs  b t (stripXAnnot x)
        XApp   x1 x2    -> XApp (stripXAnnot x1) (stripXAnnot x2)
        XDefix xs       -> XDefix (map stripXAnnot xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


-------------------------------------------------------------------------------
-- | Build sequence of applications.
makeXApps  :: GExp l -> [GExp l] -> GExp l
makeXApps t1 ts
        = foldl XApp t1 ts


-- | Flatten an application into the functional expression and its arguments,
--   or `Nothing if this is not an application.
takeXApps :: GExp l -> Maybe (GExp l, [GExp l])
takeXApps xx
 = case xx of
        XApp x1@XApp{} a2
         -> case takeXApps x1 of
                Just (f1, as1)  -> Just (f1, as1 ++ [a2])
                Nothing         -> Nothing

        XApp x1 a2
         -> Just (x1, [a2])

        _                       -> Nothing

