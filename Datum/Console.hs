
-- | Utilities for working with datum trees in the GHCi console.
module Datum.Console
        ( Dump  (..)
        , Check (..)
        , Save  (..))
where
import Datum.Data.Tree
import Datum.Data.Tree.SExp
import Text.PrettyPrint.Leijen
import qualified System.IO              as System


-- Dumping -------------------------------------------------------------------
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


-- Check ---------------------------------------------------------------------
class Check a where
 type Check' a
 -- | Type check an object.
 check :: a -> IO (Check' a)


-- Tree
instance Check (Tree a) where
 type Check' (Tree a) = Tree 'O
 check t
  = case checkTree t of
        Left err        
         -> do  putDoc $ ppError err <> line
                error "Tree is not well formed."

        Right t'        
         ->     return t'


-- Forest
instance Check (Forest a) where
 type Check' (Forest a) = Forest 'O
 check t
  = case checkForest t of
        Left err        
         -> do  putDoc $ ppError err <> line
                error "Forest is not well formed."

        Right t'        
         ->     return t'


-- Saving ---------------------------------------------------------------------
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

