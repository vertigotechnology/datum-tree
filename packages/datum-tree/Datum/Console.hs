
-- | Utilities for working with datum trees in the GHCi console.
module Datum.Console
        ( Dump  (..)
        , CheckIO (..)
        , Save  (..)
        , initial
        , sample
        , loadCSV
        , XSV.HasHeader (..)

        , module SExp
        , module Control.Monad
        , module Data.Maybe)
where
import Datum.Console.Dump
import Datum.Console.Check
import Datum.Console.Save

import Datum.Data.Tree
import Datum.Data.Tree.Codec.XSV                as XSV
import Datum.Data.Tree.Codec.SExp               as SExp

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8     as BS8


-- | Load a CSV file as a tree.
loadCSV :: FilePath -> IO (Tree 'O)
loadCSV path
 = do   bs              <- BS8.readFile path
        let Right t     =  XSV.decodeXSV ',' XSV.HasHeader bs
        return  t
