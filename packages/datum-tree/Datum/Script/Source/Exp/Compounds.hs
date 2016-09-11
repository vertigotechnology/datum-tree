
module Datum.Script.Source.Exp.Compounds
        ( -- * Modules
          globModules
        , extractExpOfModule

          -- * Annotations
        , stripXAnnotM
        , stripXAnnotT
        , stripXAnnotX

          -- * Abstractions
        , makeXAbss

          -- * Applications
        , makeXApps
        , takeXApps)
where
import Datum.Script.Source.Exp.Generic
import Data.Text                        (Text)
import qualified Data.Text              as Text
import qualified Data.Map.Strict        as Map


-------------------------------------------------------------------------------
-- | Glob two modules together.
--
--   The result contains all the top-level things from both modules.
--
globModules :: GModule l -> GModule l -> GModule l
globModules (Module ts1) (Module ts2)
        = Module (ts1 ++ ts2)


-- Extract an expression that evaluates the script in the given module.
--   The module must have a 'main' entry point, else `Nothing`.
extractExpOfModule
        :: GXBind l ~ Text
        => GModule l -> Maybe (GExp l)

extractExpOfModule mm
 = let  tops    = moduleTops mm

        supers  = Map.fromList 
                $ [(b, makeXAbss bts xBody) | TBind b bts xBody <- tops]

        tMain           = Text.pack "main"
        supersNonMain   = Map.delete tMain supers

   in   case Map.lookup tMain supers of
         Nothing        
          -> Nothing

         Just xMain     
          -> Just $ XRec (Map.toList supersNonMain) xMain


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
 = let down = stripXAnnotX
   in case xx of
        XAnnot _ x      -> down x
        XPrim{}         -> xx
        XFrag{}         -> xx
        XVar{}          -> xx
        XCast  c x      -> XCast   c   (down x)
        XAbs   b t x    -> XAbs    b t (down x)
        XApp   x1 x2    -> XApp   (down x1) (down x2)
        XRec   bxs x2   -> XRec   [(b, down x) | (b, x) <- bxs] (down x2)
        XIf    x1 x2 x3 -> XIf    (down x1) (down x2) (down x3)
        XDo    ss x     -> XDo    (map stripXAnnotS ss) (down x)
        XDefix xs       -> XDefix (map down xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


-- | Strip annotations from a source statement.
stripXAnnotS :: GStmt l -> GStmt l
stripXAnnotS ss
 = case ss of
        SStmt x         -> SStmt (stripXAnnotX x)
        SBind b x       -> SBind b (stripXAnnotX x)


-------------------------------------------------------------------------------
-- | Make some nested abstractions
makeXAbss 
        :: [(GXBind l, Maybe (GExp l))] -- ^ Parameter names and optional types.
        -> GExp l                       -- ^ Body of the abstraction.
        -> GExp l
makeXAbss bts x = foldr (\(b, t) x' -> XAbs b t x') x bts


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

