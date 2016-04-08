
module Datum.Console.Save
        (Save (..))
where
import Datum.Console.Check

import Datum.Data.Tree.SExp
import Datum.Data.Tree.Exp
import Text.PrettyPrint.Leijen
import qualified System.IO      as System


class Save a where
 -- | Save an object to a file.
 save :: FilePath -> a -> IO ()


-- Tree
instance Save (Tree a) where
 save file t
  = do  t'      <- check t
        System.withFile file System.WriteMode
         $ \h -> hPutDoc h (ppTree t' <> line)


-- Forest
instance Save (Forest a) where
 save file t
  = do  t'      <- check t
        System.withFile file System.WriteMode
         $ \h -> hPutDoc h (ppForest t' <> line)

