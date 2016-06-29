
module Datum.Script.Source.Transform.ToCore
        (toCoreX)
where
import Data.Text                                (Text)
import qualified Datum.Script.Source.Exp        as S
import qualified Datum.Script.Core.Exp          as C

-- | Things that can go wrong when converting to core.
data Error
        = ErrorSugaredInfix S.Exp

deriving instance Show Error


-- | Convert a source expression to core.
toCoreX :: S.Exp -> Either Error C.Exp
toCoreX xx
 = case xx of
        S.XAnnot _ x    -> toCoreX x
        S.XPrim p       -> C.XPrim <$> toCorePrim  p
        S.XVar  u       -> C.XVar  <$> toCoreBound u
        S.XCast c x     -> C.XCast <$> toCoreCast  c  <*> toCoreX x
        S.XAbs  b  mt x -> C.XAbs  <$> toCoreBind  b  <*> toCoreMT mt <*> toCoreX x
        S.XApp  x1 x2   -> C.XApp  <$> toCoreX     x1 <*> toCoreX x2

        S.XLet  b mt x1 x2
         -> C.XApp 
                <$> (C.XAbs 
                        <$> toCoreBind b
                        <*> toCoreMT mt
                        <*> toCoreX x1)
                <*> toCoreX x2


        S.XDefix{}      -> Left $ ErrorSugaredInfix xx
        S.XInfixOp{}    -> Left $ ErrorSugaredInfix xx
        S.XInfixVar{}   -> Left $ ErrorSugaredInfix xx


-- | Convert a source bind to core.
toCoreBind  :: Text -> Either Error C.Bind
toCoreBind tt
        = return $ C.BName tt


-- | Convert a source bound to core.
toCoreBound :: Text -> Either Error C.Bound
toCoreBound tt
        = return $ C.UName tt


-- | Convert a source cast to core.
toCoreCast :: S.Cast -> Either Error C.Cast
toCoreCast cc
 = case cc of
        S.CRun          -> return $ C.CRun
        S.CBox          -> return $ C.CBox


-- | Convert an optional source type to core,
--   if no type annotation is available we use a hole.
toCoreMT :: Maybe S.Exp -> Either Error C.Exp
toCoreMT mt
 = case mt of
        Nothing         -> return $ C.XPrim (C.PHole (C.XPrim (C.PType 2)))
        Just t          -> toCoreX t


-- | Convert a source primitive to core.
toCorePrim :: S.Prim -> Either Error C.Prim
toCorePrim pp
 = case pp of
        -- Universal
        S.PHole t       -> C.PHole <$> toCoreX t
        S.PType i       -> return $ C.PType i
        S.PFun  i       -> return $ C.PFun  i

        -- Kinds (level 2)
        S.PKComp        -> return $ C.PKComp
        S.PKData        -> return $ C.PKData
        S.PKAtom        -> return $ C.PKAtom

        -- Types (level 1)
        S.PTS           -> return $ C.PTS
        S.PTList        -> return $ C.PTList

        S.PTName        -> return $ C.PTName
        S.PTTree        -> return $ C.PTTree
        S.PTTreePath    -> return $ C.PTTreePath
        S.PTFilePath    -> return $ C.PTFilePath

        S.PTAtom t      -> return $ C.PTAtom t

        S.PVName t      -> return $ C.PVName t
        S.PVList x xs   -> C.PVList <$> toCoreX x <*> mapM toCoreX xs
        S.PVTree t      -> return $ C.PVTree t
        S.PVTreePath ts -> return $ C.PVTreePath ts
        S.PVFilePath f  -> return $ C.PVFilePath f

        S.PVAtom a      -> return $ C.PVAtom a
        S.PVOp   p      -> return $ C.PVOp   p

