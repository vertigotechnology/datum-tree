
module Datum.Script.Source.Exp
        ( -- * Binding
          Name

          -- * Syntax
        , Source (..)

          -- ** Top-level definitions
        , Module, GModule (..)
        , Top,    GTop    (..)

          -- ** Expressions
        , Exp, GExp (..)

          -- ** Casts
        , Cast (..)

          -- ** Primitives
        , Prim, GPrim (..)
        , T.AtomType    (..)
        , T.Atom        (..)

          -- ** Generics
        , GXAnnot, GXPrim
        , GXBound, GXBind
        , GXCast
        , type ShowGExp

        -- * Compounds
        , globModules
        , stripXAnnotM, stripXAnnotT, stripXAnnotX
        , makeXApps
        , takeXApps

        -- ** Pattern Synonyms
        , module Datum.Script.Core.Exp.Prim)
where
import Datum.Script.Core.Exp.Bind
import Datum.Script.Core.Exp.Cast
import Datum.Script.Core.Exp.Prim
import Datum.Script.Core.Exp.Generic    ()
import Datum.Script.Source.Exp.Compounds
import Datum.Script.Source.Exp.Generic
import Text.Parsec                      (SourcePos)
import qualified Datum.Data.Tree.Exp    as T


-- | Tag for the core lanugage with a unit annotation.
data Source     = Source
type Module     = GModule Source
type Top        = GTop    Source
type Exp        = GExp    Source
type Prim       = GPrim (GExp Source)

type instance GXAnnot Source = SourcePos
type instance GXPrim  Source = Prim
type instance GXBind  Source = Name
type instance GXBound Source = Name
type instance GXCast  Source = Cast
