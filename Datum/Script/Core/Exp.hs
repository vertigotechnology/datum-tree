
module Datum.Script.Core.Exp
        ( Core (..)
        , GExp (..),  Exp
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
import Datum.Script.Core.Exp.Bind
import Datum.Script.Core.Exp.Cast
import Datum.Script.Core.Exp.Prim
import Datum.Script.Core.Exp.Generic
import qualified Datum.Data.Tree.Exp    as T
import Data.Text                        (Text)


-- | Tag for the core lanugage with a unit annotation.
data Core       = Core
type Exp        = GExp Core
type Prim       = GPrim Core

type instance GXAnnot Core = ()
type instance GXPrim  Core = Prim
type instance GXBind  Core = Bind
type instance GXBound Core = Bound
type instance GXCast  Core = Cast
