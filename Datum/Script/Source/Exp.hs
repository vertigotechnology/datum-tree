
module Datum.Script.Source.Exp
        ( Source (..)
        , GExp   (..),  Exp
        , GPrim  (..), Prim

        , GXAnnot, GXPrim
        , GXBound, GXBind
        , GXCast

        , Name
        , Bind (..), Bound (..)
        , Cast (..)
        , T.AtomType    (..)
        , T.Atom        (..)

        , module Datum.Script.Core.Exp.Prim)
where
import Datum.Script.Core.Exp.Bind
import Datum.Script.Core.Exp.Cast
import Datum.Script.Core.Exp.Prim
import Datum.Script.Core.Exp.Generic    ()
import Datum.Script.Source.Exp.Generic
import Text.Parsec                      (SourcePos)
import Data.Text                        (Text)
import qualified Datum.Data.Tree.Exp    as T


-- | Tag for the core lanugage with a unit annotation.
data Source     = Source
type Exp        = GExp  Source
type Prim       = GPrim (GExp Source)

type instance GXAnnot Source = SourcePos
type instance GXPrim  Source = Prim
type instance GXBind  Source = Bind
type instance GXBound Source = Bound
type instance GXCast  Source = Cast
