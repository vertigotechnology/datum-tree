
module Datum.Console.Check
        (Check  (..))
where
import Datum.Data.Tree.SExp
import Datum.Data.Tree.Exp
import Datum.Data.Tree
import Text.PrettyPrint.Leijen


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


