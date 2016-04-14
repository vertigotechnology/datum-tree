{-# LANGUAGE UndecidableInstances #-}
module Datum.Console.Check
        (CheckIO (..))
where
import Datum.Data.Tree.SExp
import Datum.Data.Tree.Exp
import Datum.Data.Tree          as T
import Text.PrettyPrint.Leijen


class Check a => CheckIO a where
 -- | Type check an object.
 check :: a -> IO (Checked' a)


-- Tree
instance Check   (Tree c) 
      => CheckIO (Tree c) where
 check t
  = case T.check t of
        Left err        
         -> do  putDoc $ ppError err <> line
                error "Tree is not well formed."

        Right t'        
         ->     return t'


-- Forest
instance Check   (Forest c)
      => CheckIO (Forest c) where
 check t
  = case T.check t of
        Left err        
         -> do  putDoc $ ppError err <> line
                error "Forest is not well formed."

        Right t'        
         ->     return t'


