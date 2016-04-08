
-- | Utilities for working with datum trees in the GHCi console.
module Datum.Console
        ( Dump  (..)
        , Check (..)
        , Save  (..)
        , loadCSV)
where
import Datum.Console.Dump
import Datum.Console.Check
import Datum.Console.Save

import Datum.Data.Tree
import Datum.Data.Tree.Exp
import Datum.Data.Tree.SExp
import Datum.Data.Tree.Codec
import Text.PrettyPrint.Leijen
import qualified System.IO              as System
import qualified Data.ByteString.Lazy.Char8     as BS8


loadCSV :: FilePath -> IO (Tree 'O)
loadCSV path
 = do   bs              <- BS8.readFile path
        let Right t     =  decodeCSV bs
        return  t


-- Meta ----------------------------------------------------------------------
class Meta a where
 type Meta' a 
 -- | Get the meta-data of a thing.
 meta :: a -> Meta' a

instance Meta (Tree c) where
 type Meta' (Tree c) = BranchType
 meta (Tree _ bt)    = bt




