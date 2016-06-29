{-# LANGUAGE UndecidableInstances #-}

-- | Things that can go wrong when resolving infix expressions.
module Datum.Script.Source.Transform.Defix.Error
        (Error (..))
where
import Datum.Script.Source.Exp


-- | Things that can go wrong when defixing code.
data Error l
        -- | Infix operator symbol has no infix definition.
        = ErrorNoInfixDef
        { errorAnnot            :: GXAnnot l
        , errorSymbol           :: Name }

        -- | Two non-associative operators with the same precedence.
        | ErrorDefixNonAssoc
        { errorOp1              :: Name
        , errorAnnot1           :: GXAnnot l
        , errorOp2              :: Name
        , errorAnnot2           :: GXAnnot l }

        -- | Two operators of different associativies with same precedence.
        | ErrorDefixMixedAssoc 
        { errorAnnot            :: GXAnnot l
        , errorOps              :: [Name] }

        -- | Infix expression is malformed.
        --   Eg "+ 3" or "2 + + 2"
        | ErrorMalformed
        { errorAnnot            :: GXAnnot l
        , errorExp              :: GExp l }

deriving instance ShowGExp l => Show (Error l)
