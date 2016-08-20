
module Datum.Script.Source.Exp
        ( -- * Binding
          Name

          -- * Syntax
        , Source (..)

          -- ** Top-level definitions
        , Module, S.GModule (..)
        , Top,    S.GTop    (..)

          -- ** Expressions
        , Exp,    S.GExp    (..)

          -- ** Casts
        , K.Cast (..)

          -- ** Primitives
        , Prim, GPrim (..)
        , T.AtomType    (..)
        , T.Atom        (..)

          -- ** Generics
        , S.GXAnnot, S.GXPrim
        , S.GXBound, S.GXBind
        , S.GXCast
        , type S.ShowGExp

        -- * Compounds
        , S.globModules
        , S.extractExpOfModule
        , S.stripXAnnotM, S.stripXAnnotT, S.stripXAnnotX
        , S.makeXApps
        , S.takeXApps

        -- ** Pattern Synonyms
        , module Datum.Script.Core.Exp.Prim)
where
import Text.Parsec                                      (SourcePos)
import Datum.Script.Core.Exp.Prim
import Data.Text                                        (Text)
import qualified Datum.Script.Core.Exp                  as C
import qualified Datum.Script.Kernel.Exp.Cast           as K
import qualified Datum.Script.Source.Exp.Compounds      as S
import qualified Datum.Script.Source.Exp.Generic        as S
import qualified Datum.Data.Tree.Exp                    as T

type Name       = Text

-- | Tag for the core lanugage with a unit annotation.
data Source     = Source
type Module     = S.GModule Source
type Top        = S.GTop    Source
type Exp        = S.GExp    Source
type Prim       = C.GPrim (S.GExp Source)

type instance S.GXAnnot Source = SourcePos
type instance S.GXPrim  Source = Prim
type instance S.GXBind  Source = Name
type instance S.GXBound Source = Name
type instance S.GXCast  Source = K.Cast
