
module Datum.Script.Core.Exp.Bind
        ( Name
        , Bind  (..)
        , Bound (..))
where
import Data.Text                (Text)


-- | Names of variables.
type Name
        = Text


-- | Binding occurrences of variables.
data Bind
        -- | An anonymous binder.
        = BAnon

        -- | A named binder.
        | BName Name

deriving instance Show Bind
deriving instance Eq   Bind


-- | Bound occurrences of variables.
data Bound
        -- | A debruijn index.
        = UIx   Int

        -- | A named variable.
        | UName Name

deriving instance Show Bound
deriving instance Eq   Bound

