
module Datum.Console.Dump
        (Dump   (..))
where
import qualified Datum.Console.Check    as C

import Datum.Data.Tree.SExp
import Datum.Data.Tree.Exp
import Datum.Data.Tree                  as T
import Text.PrettyPrint.Leijen


class Dump a where
 -- | Dump an object to console.
 dump :: a -> IO ()


-- Trees ----------------------------------------------------------------------
instance Dump (Tree 'O) where
 dump t = putDoc $ (ppTree t <> line)


instance Dump (Tree 'X) where
 dump t 
  = case T.check t of
        Left err        -> putDoc $ (ppError err <> line)
        Right t'        -> dump t'


instance Dump [Tree 'O] where
 dump ts
  = do  mapM_ dump ts


instance Dump [Tree 'X] where
 dump ts
  = do  ts'     <- mapM C.check ts
        mapM_ dump ts'


-- Forests --------------------------------------------------------------------
instance Dump (Forest 'O) where
 dump t = putDoc $ (ppForest t <> line)


instance Dump (Forest 'X) where
 dump t 
  = case T.check t of
        Left err        -> putDoc $ (ppError err <> line)
        Right t'        -> dump t'


instance Dump [Forest 'O] where
 dump fs
  = do  mapM_ dump fs


instance Dump [Forest 'X] where
 dump fs
  = do  fs'     <- mapM C.check fs
        mapM_ dump fs'


-- Key ------------------------------------------------------------------------
instance Dump [Key 'O] where
 dump ks = putDoc $ (ppKeyList ks <> line)


instance Dump [Key 'X] where
 dump ks
  = case mapM T.check ks of
        Left err        -> putDoc $ (ppError err <> line)
        Right ks'       -> dump ks'


-- Element -------------------------------------------------------------------


-- Meta ----------------------------------------------------------------------
instance Dump BranchType where
 dump bt = putDoc $ ppBranchType bt <> line

instance Dump TupleType where
 dump tt = putDoc $ ppTupleType tt <> line

instance Dump AtomType where
 dump tt = putDoc $ ppAtomType tt <> line


-- Data -----------------------------------------------------------------------
instance Dump Branch where
 dump b = putDoc $ ppBranch b <> line

instance Dump Group where
 dump g = putDoc $ ppGroup g <> line

instance Dump Tuple where
 dump t = putDoc $ ppTuple t <> line

instance Dump Atom where
 dump a = putDoc $ ppAtom a <> line


