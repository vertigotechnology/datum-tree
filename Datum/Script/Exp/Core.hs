
module Datum.Script.Exp.Core 
        ( Core (..)
        , GExp (..), Exp
        , GPrim (..), Prim

        , GXAnnot, GXPrim
        , GXBound, GXBind
        , GXCast

        , Name
        , Bind (..), Bound (..)
        , Cast (..)
        , T.AtomType    (..)
        , T.Atom        (..)

        , module Datum.Script.Exp.Prim)
where
import Datum.Script.Exp.Generic
import Datum.Script.Exp.Prim
import qualified Datum.Data.Tree.Exp    as T
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
-- | Tag for the core lanugage with a unit annotation.
data Core       = Core
type Exp        = GExp Core
type Prim       = GPrim Core

type instance GXAnnot Core = ()
type instance GXPrim  Core = Prim
type instance GXBind  Core = Bind
type instance GXBound Core = Bound
type instance GXCast  Core = Cast


---------------------------------------------------------------------------------------------------
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


-- | Type casts.
data Cast
        -- | Run an effectful computation.
        = CRun

        -- | Box up an effectful expression.
        | CBox

deriving instance Show Cast
deriving instance Eq   Cast




