
module Datum.Script.Core.Exp
        ( -- * Binding
          Bind
        , pattern BAnon
        , pattern BName

        , Bound
        , pattern UIx
        , pattern UName


        , Name

          -- * Syntax
        , Core (..)

          -- ** Expressions
        , Exp,  K.GExp (..)

          -- ** Casts
        , K.Cast  (..)

          -- ** Primitives
        , Prim,         K.GPrim  (..)
        , Frag,         C.GCPrim (..)
        , T.AtomType    (..)
        , T.Atom        (..)

          -- * Generics
        , K.GXAnnot, K.GXPrim
        , K.GXBound, K.GXBind
        , K.GXCast

          -- * Compounds
        , K.makeXAbss
        , K.makeXApps
        , K.takeXApps, K.takeXApps'
        , K.expOfPipeline

          -- ** Pattern Synonyms
        , module Datum.Script.Core.Exp.Prim
        , module Datum.Script.Kernel.Exp.Prim)
where
import Datum.Script.Core.Exp.Prim
import Datum.Script.Kernel.Exp.Prim                     hiding ((~>), (~~>), typeOfPrim)
import Data.Text                                        (Text)
import qualified Datum.Script.Core.Exp.Prim             as C
import qualified Datum.Script.Kernel.Exp.Bind           as K
import qualified Datum.Script.Kernel.Exp.Cast           as K
import qualified Datum.Script.Kernel.Exp.Generic        as K
import qualified Datum.Script.Kernel.Exp.Compounds      as K
import qualified Datum.Script.Kernel.Exp.Prim           as K
import qualified Datum.Data.Tree                        as T
import qualified Datum.Data.Tree.Exp                    as T


-- | Names of variables.
type Name       = Text

type Bind       = K.Bind  Name
pattern BAnon   = K.BAnon
pattern BName n = K.BName n

type Bound      = K.Bound Name
pattern UIx   i = K.UIx   i
pattern UName n = K.UName n

-- | Tag for the core lanugage with a unit annotation.
data Core       = Core
type Exp        = K.GExp   Core
type Prim       = K.GPrim  Exp
type Frag       = C.GCPrim Exp

type instance K.GXAnnot Core = ()
type instance K.GXPrim  Core = Prim
type instance K.GXBind  Core = Bind
type instance K.GXBound Core = Bound
type instance K.GXCast  Core = K.Cast
type instance K.GXFrag  Core = Frag

deriving instance Show Core

