{-# LANGUAGE UndecidableInstances #-}

module Datum.Script.Kernel.Exp.Prim where
import Datum.Script.Kernel.Exp.Generic


---------------------------------------------------------------------------------------------------
-- | Primitive objects in the kernel language.
data GPrim x
        -- Universal, works at all levels.
        = PHole x               -- ^ A hole of the given type, to be elaborated.
        | PType Int             -- ^ Type of types at the given level.
        | PFun  Int             -- ^ Function arrow at the given level.
        | PAll  Int x x
                                -- ^ Universal quantification with a kind and bound.

        -- Baked in Kinds
        | PKData                -- ^ Kind of data types.
        | PKEffect              -- ^ Kind of effect types.

        -- Baked in Types
        | PTS                   -- ^ State computation type constructor.
        | PTVoid                -- ^ Void type constructor.
        | PTUnit                -- ^ Unit type constructor.

        -- Baked in Values
        | PVUnit                -- ^ Unit value.


deriving instance (Show x) => Show (GPrim x)


---------------------------------------------------------------------------------------------------
typeOfPrim 
        :: (x ~ GExp l, GXPrim l ~ GPrim l)
        => GPrim x
        -> GExp  l

typeOfPrim pp
 = case pp of
        -- Types of Generic things.
        PHole t         
         -> t
        
        PType n         
         -> XType (n + 1)

        PFun  n
         -> let n' = n + 1
            in  XFun n' (XType n') (XFun n' (XType n') (XType n'))

        PAll  n k t
         -> let n'      = n + 1
            in  XFun n' k (XFun n' t (XType n'))

        -- Types of Kinds
        PKData          -> XType 2
        PKEffect        -> XType 2

        -- Types of Types
        PTS             -> XKEffect ~~> XKData ~~> XKData
        PTVoid          -> XKData
        PTUnit          -> XKData

        -- Types of Values
        PVUnit          -> XTUnit


---------------------------------------------------------------------------------------------------
-- Generic
pattern XType n         = XPrim (PType n)
pattern XFun  n a b     = XApp  (XApp (XPrim (PFun n)) a) b

-- Kinds
pattern XKData          = XPrim PKData
pattern XKEffect        = XPrim PKEffect


-- Types
pattern XTS a           = XApp (XPrim PTS) a
pattern XTUnit          = XPrim PTUnit
pattern XTVoid          = XPrim PTVoid

-- Values
pattern XVUnit          = XPrim PVUnit


(@@) a b  = XApp a b
infixl 9 @@

(~>) a b  = XApp (XApp (XPrim (PFun 1)) a) b
infixr ~>

(~~>) a b = XApp (XApp (XPrim (PFun 2)) a) b
infixr ~~>

