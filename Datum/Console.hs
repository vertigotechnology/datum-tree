module Datum.Console
where
import Datum.Data.Tree
import Datum.Data.Tree.SExp
import Text.PrettyPrint.Leijen

class Dump a where
 dump :: a -> IO ()


-- Trees ----------------------------------------------------------------------
instance Dump (Tree O) where
 dump t = putDoc $ ppTree t


instance Dump (Tree X) where
 dump t 
  = case checkTree t of
        Left err        -> putDoc $ ppError err
        Right t'        -> dump t'


-- Key Lists ------------------------------------------------------------------
instance Dump [Key O] where
 dump ks = putDoc $ ppKeyList ks


instance Dump [Key X] where
 dump ks
  = case mapM checkKey ks of
        Left err        -> putDoc $ ppError err
        Right ks'       -> dump ks