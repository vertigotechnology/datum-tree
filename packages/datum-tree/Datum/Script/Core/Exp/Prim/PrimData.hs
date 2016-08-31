
module Datum.Script.Core.Exp.Prim.PrimData
        ( PrimData  (..)
        , PrimField (..))
where
import Data.Text                        (Text)
import qualified Datum.Data.Tree.Exp    as T


-- | Primitive data values, excluding atomic values that
--   come with the definition of trees.
data PrimData x
        = PDType     !x                 -- ^ Treat a closed type as a data value.
        | PDName     !Text              -- ^ Field or branch name.
        | PDAtom     !T.Atom            -- ^ Atomic value.
        | PDTreePath ![Text]            -- ^ Datum tree path.
        | PDFilePath !FilePath          -- ^ File path.
        | PDRecord   ![PrimField x]     -- ^ Record of several fields.
        | PDArray    !x ![PrimData x]   -- ^ Array of elements of the given type.
        | PDTree     !(T.Tree   'T.O)   -- ^ Checked datum tree.
        | PDForest   !(T.Forest 'T.O)   -- ^ Checked datum forest.
        deriving Show


-- | A field of a record.
data PrimField x
        = PFField    
        { pffieldName   :: Text 
        , pffieldType   :: Maybe x
        , pffieldValue  :: PrimData x }
        deriving Show

