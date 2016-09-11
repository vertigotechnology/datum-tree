
module Datum.Script.Kernel.Exp.Compounds
        ( stripXAnnot

          -- * Abstractions
        , makeXAbss

          -- * Applications
        , makeXApps
        , takeXApps, takeXApps'

          -- * Foralls
        , makeXForall

          -- *Pipelines
        , expOfPipeline)
where
import Datum.Script.Kernel.Exp.Generic
import Datum.Script.Kernel.Exp.Prim
import Datum.Script.Kernel.Exp.Bind

-------------------------------------------------------------------------------
-- | Strip annotations from a source expression.
stripXAnnot :: GExp l -> GExp l
stripXAnnot xx
 = let down = stripXAnnot
   in case xx of
        XAnnot _ x      -> down x
        XVar{}          -> xx
        XAbs   b t x    -> XAbs  b t (down x)
        XApp   x1 x2    -> XApp (down x1) (down x2)
        XRec   bxs x2   -> XRec [(b, down x) | (b, x) <- bxs] (down x2)
        XCast  c x      -> XCast c   (down x)
        XIf    x1 x2 x3 -> XIf   (down x1) (down x2) (down x3)
        XPrim{}         -> xx
        XFrag{}         -> xx
        

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
-- | Make a Forall type.
makeXForall 
        :: ( GXPrim  l ~ GPrim (GExp l)
           , GXBind  l ~ Bind  n
           , GXBound l ~ Bound n)
        => GExp l -> GExp l -> (GExp l -> GExp l) -> GExp l
makeXForall k b f
        = XApp (XPrim (PAll 1 k b)) (XAbs BAnon k (f (XVar (UIx 0))))


-------------------------------------------------------------------------------
-- | Combine individule pipeline stages into a combined expression.
expOfPipeline :: [GExp l] -> Maybe (GExp l)
expOfPipeline xx
 = make (reverse xx)
 where  make [] = Nothing
        make xs = Just (foldr1 XApp xs)


