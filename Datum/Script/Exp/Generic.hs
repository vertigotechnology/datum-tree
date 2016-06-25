
module Datum.Script.Exp.Generic where


type family GXAnnot l
type family GXPrim  l
type family GXBind  l
type family GXBound l


-- | Generic expression language.
data GExp l
        -- | Annotated expression.
        = XAnnot (GXAnnot l) (GExp l)

        -- | Primitive constant or operator.
        | XPrim  (GXPrim  l)

        -- | Bound variable.
        | XVar   (GXBound l)

        -- | Function abstraction with a type and body.
        | XAbs   (GXBind  l) (GExp l) (GExp l)

        -- | Function application.
        | XApp   (GExp l)    (GExp l)

