
module Datum.Script.Core.Exp.Compounds
        ( stripXAnnot
        , makeXAbss
        , makeXApps
        , takeXApps, takeXApps'
        , expOfPipeline)
where
import Datum.Script.Core.Exp.Generic


-------------------------------------------------------------------------------
-- | Strip annotations from a source expression.
stripXAnnot :: GExp l -> GExp l
stripXAnnot xx
 = let down = stripXAnnot
   in case xx of
        XAnnot _ x        -> down x
        XPrim{}           -> xx
        XVar{}            -> xx
        XCast  c x        -> XCast c   (down x)
        XAbs   b t x      -> XAbs  b t (down x)
        XApp   x1 x2      -> XApp (down x1) (down x2)


-------------------------------------------------------------------------------
-- | Make some nested abstractions
makeXAbss 
        :: [(GXBind l, GExp l)]         -- ^ Parameter names and types.
        -> GExp l                       -- ^ Body of the abstraction.
        -> GExp l

makeXAbss bts x
        = foldr (\(b, t) x' -> XAbs b t x') x bts


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


-- | Like `takeXApps`, but we take both sides of an already desconstructed
--   application node.
takeXApps' :: GExp l -> GExp l -> (GExp l, [GExp l])
takeXApps' x1 x2
 = case x1 of 
        XApp x11 x12
         -> let (x1f, x1s)      = takeXApps' x11 x12
            in  (x1f, x1s ++ [x12])

        _ -> (x1, [x2])


-------------------------------------------------------------------------------
-- | Combine individule pipeline stages into a combined expression.
expOfPipeline :: [GExp l] -> Maybe (GExp l)
expOfPipeline xx
 = make (reverse xx)
 where  make [] = Nothing
        make xs = Just (foldr1 XApp xs)


