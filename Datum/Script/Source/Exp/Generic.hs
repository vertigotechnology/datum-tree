
{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Source.Exp.Generic where


type family GXAnnot l
type family GXPrim  l
type family GXBind  l
type family GXBound l
type family GXCast  l


-- | Generic expression language.
data GExp l

        -- Core Constructors ----------------------------------------
        --   These form the core language.

        -- | Annotated expression.
        = XAnnot !(GXAnnot l) !(GExp l)

        -- | Primitive constant or operator.
        | XPrim  !(GXPrim  l)

        -- | Bound variable.
        | XVar   !(GXBound l)

        -- | Type cast.
        | XCast  !(GXCast  l) !(GExp l)

        -- | Function abstraction with a type and body.
        | XAbs   !(GXBind  l) !(GExp l) !(GExp l)

        -- | Function application.
        | XApp   !(GExp    l) !(GExp l)

        -- Sugar Constructors ---------------------------------------
        --   These define syntactic sugar in the source language,
        --   which is removed when transforming to the core language.

        -- | An infix expression that needs to have infix ops removed.
        | XDefix    ![GExp l]

        -- | Use of a naked infix operator, like in 1 + 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixOp  !(GXBound l) 

        -- | Use of an infix operator as a plain variable, like in (+) 1 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixVar !(GXBound l)


type ShowGExp l
        = ( Show (GXAnnot l), Show (GXPrim  l)
          , Show (GXBind  l), Show (GXBound l)
          , Show (GXCast  l))


deriving instance ShowGExp l => Show (GExp l)

