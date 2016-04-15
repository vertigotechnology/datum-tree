
-- | Utilities for working with datum trees in the GHCi console.
module Datum.Console
        ( Dump  (..)
        , CheckIO (..)
        , Save  (..)
        , initial
        , sample

        , loadCSV
        , HasHeader (..))
where
import Datum.Console.Dump
import Datum.Console.Check
import Datum.Console.Save

import Datum.Data.Tree
import Datum.Data.Tree.Codec
import qualified Data.ByteString.Lazy.Char8     as BS8


-- | Load a CSV file as a tree.
loadCSV :: FilePath -> IO (Tree 'O)
loadCSV path
 = do   bs              <- BS8.readFile path
        let Right t     =  decodeCSV HasHeader bs
        return  t
