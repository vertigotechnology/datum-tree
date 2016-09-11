
{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Source.Exp.Generic where


type family GXAnnot l
type family GXBind  l
type family GXBound l
type family GXCast  l
type family GXPrim  l
type family GXFrag  l


-- | A complete module.
data GModule l
        = Module
        { moduleTops    :: [GTop l] }

deriving instance ShowGExp l => Show (GModule l)


-- | Top level definition.
data GTop l
        -- | Top level function binding with arguments and body.
        = TBind !(GXBind l) [(GXBind l, Maybe (GExp l))] (GExp l)

deriving instance ShowGExp l => Show (GTop l)


-- | Generic expression language.
data GExp l

        -- Core Constructors ----------------------------------------
        --   These form the core language.

        -- | Annotated expression.
        = XAnnot   !(GXAnnot l) !(GExp l)

        -- | Bound variable.
        | XVar     !(GXBound l)

        -- | Function abstraction with optional type and body.
        | XAbs     !(GXBind  l) !(Maybe (GExp l)) !(GExp l)

        -- | Function application.
        | XApp     !(GExp    l) !(GExp l)

        -- | Recursive let-bindings.
        | XRec     ![(GXBind l, GExp l)] !(GExp l)

        -- | Type   cast.
        | XCast    !(GXCast  l) !(GExp l)

        -- | Selection.
        | XIf      !(GExp l) !(GExp l) !(GExp l)

        -- | Ambient primitive.
        | XPrim    !(GXPrim  l)

        -- | Fragment specific primitive.
        | XFrag    !(GXFrag  l)

        -- Sugar Constructors ---------------------------------------
        --   These define syntactic sugar in the source language,
        --   which is removed when transforming to the core language.
        | XDo       ![GStmt l] !(GExp l)

        -- | An infix expression that needs to have infix ops removed.
        | XDefix    ![GExp l]

        -- | Use of a naked infix operator, like in 1 + 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixOp  !(GXBound l) 

        -- | Use of an infix operator as a plain variable, like in (+) 1 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixVar !(GXBound l)


data GStmt l
        = SStmt     (GExp l)
        | SBind     (GXBind l) (GExp l)


type ShowGExp l
        = ( Show (GXAnnot l), Show (GXPrim  l)
          , Show (GXBind  l), Show (GXBound l)
          , Show (GXCast  l), Show (GXFrag  l)
          , Show (GStmt l))


deriving instance ShowGExp l => Show (GExp l)
deriving instance ShowGExp l => Show (GStmt l)

