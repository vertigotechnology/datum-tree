{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Core.Exp.Generic where


type family GXAnnot l
type family GXPrim  l
type family GXBind  l
type family GXBound l
type family GXCast  l


-- | Generic expression language.
data GExp l
        -- | Annotated expression.
        = XAnnot (GXAnnot l) (GExp l)

        -- | Primitive constant or operator.
        | XPrim  (GXPrim  l)

        -- | Bound variable.
        | XVar   (GXBound l)

        -- | Type cast.
        | XCast  (GXCast  l) (GExp l)

        -- | Function abstraction with a type and body.
        | XAbs   (GXBind  l) (GExp l) (GExp l)

        -- | Function application.
        | XApp   (GExp    l) (GExp l)

        -- | Group of recursive let-bindings.
        | XRec   [(GXBind l, GExp l)] (GExp l)



type ShowGExp l
        = ( Show (GXAnnot l), Show (GXPrim  l)
          , Show (GXBind  l), Show (GXBound l)
          , Show (GXCast  l))


deriving instance ShowGExp l => Show (GExp l)

