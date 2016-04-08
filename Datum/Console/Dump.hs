
module Datum.Console.Dump
        (Dump   (..))
where
import Datum.Console.Check

import Datum.Data.Tree.SExp
import Datum.Data.Tree.Exp
import Datum.Data.Tree
import Text.PrettyPrint.Leijen


class Dump a where
 -- | Dump an object to console.
 dump :: a -> IO ()


-- Trees
instance Dump (Tree 'O) where
 dump t = putDoc $ (ppTree t <> line)


instance Dump (Tree 'X) where
 dump t 
  = case checkTree t of
        Left err        -> putDoc $ (ppError err <> line)
        Right t'        -> dump t'


instance Dump [Tree 'O] where
 dump ts
  = do  mapM_ dump ts


instance Dump [Tree 'X] where
 dump ts
  = do  ts'     <- mapM check ts
        mapM_ dump ts'


-- Forests
instance Dump (Forest 'O) where
 dump t = putDoc $ (ppForest t <> line)


instance Dump (Forest 'X) where
 dump t 
  = case checkForest t of
        Left err        -> putDoc $ (ppError err <> line)
        Right t'        -> dump t'


instance Dump [Forest 'O] where
 dump fs
  = do  mapM_ dump fs


instance Dump [Forest 'X] where
 dump fs
  = do  fs'     <- mapM check fs
        mapM_ dump fs'


-- Key
instance Dump [Key 'O] where
 dump ks = putDoc $ (ppKeyList ks <> line)


instance Dump [Key 'X] where
 dump ks
  = case mapM checkKey ks of
        Left err        -> putDoc $ (ppError err <> line)
        Right ks'       -> dump ks'


-- BranchType
instance Dump BranchType where
 dump bt = putDoc $ ppBranchType bt <> line

