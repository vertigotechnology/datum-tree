{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Kernel.Exp.Generic where


type family GXAnnot l
type family GXPrim  l
type family GXBind  l
type family GXBound l
type family GXCast  l
type family GXFrag  l


-- | Generic expression language.
data GExp l
        -- | Annotated expression.
        = XAnnot (GXAnnot l) (GExp l)

        -- | Bound variable.
        | XVar   (GXBound l)

        -- | Function abstraction with a type and body.
        | XAbs   (GXBind  l) (GExp l) (GExp l)

        -- | Function application.
        | XApp   (GExp    l) (GExp l)

        -- | Group of recursive let-bindings.
        | XRec   [(GXBind l, GExp l)] (GExp l)

        -- | Type cast.
        | XCast  (GXCast  l) (GExp l)

        -- | Ambient primitive.
        --
        --   These are baked into the calculus,
        --   and are handled specially when type checking.
        | XPrim  (GXPrim  l)

        -- | Fragment specific primitive.
        --
        --   These are specific to the runtime implementation,
        --   and are treated abstractly when type checking.
        | XFrag  (GXFrag  l)


type ShowGExp l
        = ( Show (GXAnnot l)
          , Show (GXBind  l), Show (GXBound l)
          , Show (GXCast  l)
          , Show (GXPrim  l), Show (GXFrag  l))


deriving instance ShowGExp l => Show (GExp l)

