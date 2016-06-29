
module Datum.Script.Source.Exp.Compounds
        ( -- * Modules
          globModules

          -- * Annotations
        , stripXAnnotM
        , stripXAnnotT
        , stripXAnnotX

          -- * Applications
        , makeXApps
        , takeXApps)
where
import Datum.Script.Source.Exp.Generic


-------------------------------------------------------------------------------
-- | Glob two modules together.
--
--   The result contains all the top-level things from both modules.
--
globModules :: GModule l -> GModule l -> GModule l
globModules (Module ts1) (Module ts2)
        = Module (ts1 ++ ts2)


-------------------------------------------------------------------------------
-- | Strip annotations form a module.
stripXAnnotM :: GModule l -> GModule l
stripXAnnotM mm
 = case mm of
        Module ts       -> Module (map stripXAnnotT ts)


-- | Strip annotations from a top level definition.
stripXAnnotT :: GTop l -> GTop l
stripXAnnotT tt
 = case tt of
        TBind n vs x    -> TBind n vs (stripXAnnotX x)


-- | Strip annotations from a source expression.
stripXAnnotX :: GExp l -> GExp l
stripXAnnotX xx
 = case xx of
        XAnnot _ x      -> stripXAnnotX x
        XPrim{}         -> xx
        XVar{}          -> xx
        XCast  c x      -> XCast c   (stripXAnnotX x)
        XAbs   b t x    -> XAbs  b t (stripXAnnotX x)
        XApp   x1 x2    -> XApp (stripXAnnotX x1) (stripXAnnotX x2)
        XDefix xs       -> XDefix (map stripXAnnotX xs)
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

