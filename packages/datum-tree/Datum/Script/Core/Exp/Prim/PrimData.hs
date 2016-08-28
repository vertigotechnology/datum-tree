
module Datum.Script.Core.Exp.Prim.PrimData
        (PrimData (..))
where
import Data.Text                        (Text)
import qualified Datum.Data.Tree.Exp    as T


-- | Primitive data values, excluding atomic values that
--   come with the definition of trees.
data PrimData x
        = PDName     !Text              -- ^ Field or branch name.
        | PDAtom     !T.Atom            -- ^ Atomic value.
        | PDTreePath ![Text]            -- ^ Datum tree path.
        | PDFilePath !FilePath          -- ^ File path.
        | PDArray    !x ![PrimData x]   -- ^ Array of elements of the given type.
        | PDTree     !(T.Tree   'T.O)   -- ^ Checked datum tree.
        | PDForest   !(T.Forest 'T.O)   -- ^ Checked datum forest.
        deriving (Show)

