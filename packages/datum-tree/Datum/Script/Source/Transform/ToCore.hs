{-# LANGUAGE OverloadedStrings #-}

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
        S.CRun          -> return $ C.CRun
        S.CBox          -> return $ C.CBox


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

        S.PTNum         -> return $ C.PTNum
        S.PTList        -> return $ C.PTList

        S.PTName        -> return $ C.PTName
        S.PTForest      -> return $ C.PTForest
        S.PTTree        -> return $ C.PTTree
        S.PTTreePath    -> return $ C.PTTreePath
        S.PTFilePath    -> return $ C.PTFilePath

        S.PTAtom t      -> return $ C.PTAtom t

        S.PVName t      -> return $ C.PVName t
        S.PVList x xs   -> C.PVList <$> toCoreX x <*> mapM toCoreX xs
        S.PVForest t    -> return $ C.PVForest t
        S.PVTree t      -> return $ C.PVTree   t
        S.PVTreePath ts -> return $ C.PVTreePath ts
        S.PVFilePath f  -> return $ C.PVFilePath f

        S.PVAtom a      -> return $ C.PVAtom a
        S.PVOp   p      -> return $ C.PVOp   p


-- | Convert a source bound to core, 
--   converting variables to primitive operators along the way.
--
--   The parser parses primitive operators like 'load#' as plain variables.
--   Here we detect those and produce the correct element of the 'Prim' type.
--
toCoreBoundX :: Text -> Either Error C.Exp
toCoreBoundX tt
 = let  op p  = return $ C.XFrag (C.PVOp p)
   in case tt of
        "neg#"                  -> op C.PPNeg
        "add#"                  -> op C.PPAdd
        "sub#"                  -> op C.PPSub
        "mul#"                  -> op C.PPMul
        "div#"                  -> op C.PPDiv

        "eq#"                   -> op C.PPEq
        "gt#"                   -> op C.PPGt
        "ge#"                   -> op C.PPGe
        "lt#"                   -> op C.PPLt
        "le#"                   -> op C.PPLe

        "argument#"             -> op C.PPArgument
        "load#"                 -> op C.PPLoad
        "store#"                -> op C.PPStore
        "initial#"              -> op C.PPInitial
        "final#"                -> op C.PPFinal
        "sample#"               -> op C.PPSample
        "group#"                -> op C.PPGroup
        "gather#"               -> op C.PPGather
        "rename-fields#"        -> op C.PPRenameFields
        "permute-fields#"       -> op C.PPPermuteFields
        "flatten#"              -> op C.PPFlatten

        "at#"                   -> op C.PPAt
        "on#"                   -> op C.PPOn
        
        _                       -> return $ C.XVar (C.UName tt)

