
module Datum.Script.Source.Exp.Compounds
        ( -- * Modules
          globModules
        , extractExpOfModule

          -- * Annotations
        , stripXAnnotM
        , stripXAnnotT
        , stripXAnnotX

          -- * Abstractions
         ,makeXAbss

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
                $ [(v, makeXAbss bts xBody) | TBind v bts xBody <- tops]

        tMain   = Text.pack "main"
        supersNonMain   = Map.delete tMain supers

   in   case Map.lookup tMain supers of
         Nothing        
          -> Nothing

         Just xMain     
          -> Just $ foldl (\xBody (b, xBind) -> XLet b Nothing xBind xBody) xMain
                  $ Map.toList supersNonMain


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

