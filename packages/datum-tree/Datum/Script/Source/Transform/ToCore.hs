{-# LANGUAGE OverloadedStrings #-}

module Datum.Script.Source.Transform.ToCore
        ( toCoreX
        , Error (..))
where
import Data.Text                                (Text)
import qualified Data.Text                      as Text
import qualified Datum.Script.Source.Exp        as S
import qualified Datum.Script.Core.Exp          as C


-- | Things that can go wrong when converting to core.
data Error
        -- | Found a sugared infix expression.
        = ErrorSugaredInfix S.Exp

deriving instance Show Error


-- | Convert a source expression to core.
toCoreX :: S.Exp -> Either Error C.Exp
toCoreX xx
 = case xx of
        S.XAnnot _ x    -> toCoreX x
        S.XVar  u       -> toCoreBoundX u
        S.XCast c x     -> C.XCast <$> toCoreCast  c  <*> toCoreX x
        S.XAbs  b  mt x -> C.XAbs  <$> toCoreBind  b  <*> toCoreMT mt <*> toCoreX x
        S.XApp  x1 x2   -> C.XApp  <$> toCoreX     x1 <*> toCoreX x2

        S.XRec  bxs x2  
         -> do  let (bs, xs)    = unzip bxs
                bs'     <- mapM toCoreBind bs
                xs'     <- mapM toCoreX xs
                x2'     <- toCoreX x2
                return  $ C.XRec (zip bs' xs') x2'

        S.XPrim p       -> C.XPrim <$> toCorePrim p
        S.XFrag p       -> C.XFrag <$> toCoreFrag p

        S.XDo ss0 xEnd
         -> let go (S.SBind b x : ss) 
                 = do   ss'     <- go ss
                        t'      <- toCoreMT Nothing
                        x'      <- toCoreX x
                        return  $ C.XApp (C.XAbs (C.BName b) t' ss') x'

                go (S.SStmt x   : ss)
                 = do   ss'     <- go ss
                        t'      <- toCoreMT Nothing
                        x'      <- toCoreX x
                        return  $ C.XApp (C.XAbs S.BNone     t' ss') x'
                go [] 
                 = do   toCoreX xEnd

            in  go ss0

        S.XDefix{}      -> Left $ ErrorSugaredInfix xx
        S.XInfixOp{}    -> Left $ ErrorSugaredInfix xx
        S.XInfixVar{}   -> Left $ ErrorSugaredInfix xx


-- | Convert a source bind to core.
toCoreBind  :: Text -> Either Error C.Bind
toCoreBind tt
        = return $ C.BName tt


-- | Convert a source cast to core.
toCoreCast :: S.Cast -> Either Error C.Cast
toCoreCast cc
 = case cc of
        S.CastRun       -> return $ C.CastRun
        S.CastBox       -> return $ C.CastBox


-- | Convert an optional source type to core,
--   if no type annotation is available we use a hole.
toCoreMT :: Maybe S.Exp -> Either Error C.Exp
toCoreMT mt
 = case mt of
        Nothing         -> return $ C.XPrim (C.PHole (C.XPrim (C.PType 2)))
        Just t          -> toCoreX t


-- | Convert a source ambient primitive to core.
toCorePrim :: S.Prim -> Either Error C.Prim
toCorePrim pp
 = case pp of
        -- Universal
        S.PHole t       -> C.PHole <$> toCoreX t
        S.PMeta t i     -> C.PMeta <$> toCoreX t  <*> pure i
        S.PType i       -> return $ C.PType i
        S.PFun  i       -> return $ C.PFun  i
        S.PAll  n k t   -> C.PAll n <$> toCoreX k <*> toCoreX t

        -- Kinds (level 2)
        S.PKData        -> return $ C.PKData
        S.PKEffect      -> return $ C.PKEffect

        -- Types (level 1)
        S.PTS           -> return $ C.PTS
        S.PTVoid        -> return $ C.PTVoid
        S.PTUnit        -> return $ C.PTUnit

        -- Values
        S.PVUnit        -> return $ C.PVUnit


-- | Convert a source fragment primitive to core.
toCoreFrag :: S.Frag -> Either Error C.Frag
toCoreFrag ff
 = case ff of
        S.PKAtom        -> return $ C.PKAtom
        S.PTType pt     -> C.PTType <$> toCorePrimType pt
        S.PVData d      -> C.PVData <$> toCorePrimData d
        S.PVOp   p      -> return $ C.PVOp   p


-- | Convert a primitive type from source to core.
toCorePrimType :: S.PrimType -> Either Error C.PrimType
toCorePrimType pt
 = case pt of
        S.PTNum         -> return $ C.PTNum
        S.PTName        -> return $ C.PTName
        S.PTArray       -> return $ C.PTArray
        S.PTRecord      -> return $ C.PTRecord
        S.PTForest      -> return $ C.PTForest
        S.PTTree        -> return $ C.PTTree
        S.PTTreePath    -> return $ C.PTTreePath
        S.PTFilePath    -> return $ C.PTFilePath
        S.PTValue       -> return $ C.PTValue
        S.PTAtom t      -> return $ C.PTAtom t


-- | Convert primitive data from source to core.
toCorePrimData :: S.PrimData S.Exp -> Either Error (C.PrimData C.Exp)
toCorePrimData dd
 = case dd of
        S.PDType t      -> C.PDType  <$> toCoreX t
        S.PDAtom a      -> return $ C.PDAtom a
        S.PDName t      -> return $ C.PDName t
        S.PDTreePath ts -> return $ C.PDTreePath ts
        S.PDFilePath f  -> return $ C.PDFilePath f
        S.PDRecord fs   -> C.PDRecord <$> mapM toCorePrimField fs
        S.PDArray  x xs -> C.PDArray  <$> toCoreX x <*> mapM toCorePrimData xs
        S.PDTree t      -> return $ C.PDTree   t
        S.PDForest t    -> return $ C.PDForest t


-- | Convert a primitive record field to core.
toCorePrimField :: S.PrimField S.Exp -> Either Error (C.PrimField C.Exp)
toCorePrimField ff
 = case ff of
        S.PFField n (Just t) d 
         -> C.PFField n <$> (fmap Just $ toCoreX t) <*> toCorePrimData d

        S.PFField n Nothing d 
         -> C.PFField n <$> (pure Nothing)          <*> toCorePrimData d


-- | Convert a source bound to core, 
--   converting variables to primitive operators along the way.
--
--   The parser parses primitive operators like 'load#' as plain variables.
--   Here we detect those and produce the correct element of the 'Prim' type.
--
toCoreBoundX :: Text -> Either Error C.Exp
toCoreBoundX tt
 -- Detect names of primops.
 | Just p       <- lookup (Text.unpack tt) C.primOpsOfNames
 = return $ C.XFrag (C.PVOp p)

 -- Detect names of primitive types.
 | Just p       <- lookup (Text.unpack tt) C.primTypesOfNames
 = return $ C.XFrag (C.PTType p)

 -- Detect boolean constructor names.
 | Just x       <- case Text.unpack tt of
                        "True"  -> Just $ C.XBool True
                        "False" -> Just $ C.XBool False
                        _       -> Nothing
 = return x

 -- Just a regular variable.
 | otherwise
 = return $ C.XVar (C.UName tt)

