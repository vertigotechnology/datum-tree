
module Datum.Script.Kernel.Exp.Bind
        ( Bind  (..)
        , Bound (..))
where


-- | Binding occurrences of variables.
data Bind n
        -- | An anonymous binder.
        = BAnon

        -- | A named binder.
        | BName n

deriving instance Show n => Show (Bind n)
deriving instance Eq   n => Eq   (Bind n)


-- | Bound occurrences of variables.
data Bound n
        -- | A debruijn index.
        = UIx   Int

        -- | A named variable.
        | UName n

deriving instance Show n => Show (Bound n)
deriving instance Eq   n => Eq   (Bound n)

