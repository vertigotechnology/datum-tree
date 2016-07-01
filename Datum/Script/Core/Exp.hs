
module Datum.Script.Core.Exp
        ( -- * Binding
          Bind  (..)
        , Bound (..)
        , Name

          -- * Syntax
        , Core (..)

          -- ** Expressions
        , Exp,  GExp (..)

          -- ** Casts
        , Cast  (..)

          -- ** Primitives
        , Prim, GPrim (..)
        , T.AtomType    (..)
        , T.Atom        (..)

          -- * Generics
        , GXAnnot, GXPrim
        , GXBound, GXBind
        , GXCast

          -- * Compounds
        , makeXAbss
        , makeXApps
        , takeXApps, takeXApps'
        , expOfPipeline

--          -- * Predicates
--        , isNormalOpenX

          -- ** Pattern Synonyms
        , module Datum.Script.Core.Exp.Prim)
where
import Datum.Script.Core.Exp.Bind
import Datum.Script.Core.Exp.Cast
import Datum.Script.Core.Exp.Prim
import Datum.Script.Core.Exp.Generic
import Datum.Script.Core.Exp.Compounds
-- import Datum.Script.Core.Exp.Predicates
import qualified Datum.Data.Tree        as T
import qualified Datum.Data.Tree.Exp    as T


-- | Tag for the core lanugage with a unit annotation.
data Core       = Core
type Exp        = GExp  Core
type Prim       = GPrim (GExp Core)

type instance GXAnnot Core = ()
type instance GXPrim  Core = Prim
type instance GXBind  Core = Bind
type instance GXBound Core = Bound
type instance GXCast  Core = Cast

deriving instance Show Core
