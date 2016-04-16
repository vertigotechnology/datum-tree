{-# LANGUAGE UndecidableInstances #-}
module Datum.Console.Check
        (CheckIO (..))
where
import Datum.Data.Tree.Check
import Datum.Data.Tree.Exp
import Datum.Data.Tree          as T
import Text.PrettyPrint.Leijen


class Check a => CheckIO a where
 -- | Type check an object.
 check :: a -> IO (Checked' a)

 checkIO :: String -> a -> IO (Checked' a)
 checkIO thing x
  = case T.check x of
        Left err
         -> do  putDoc $ ppError err <> line
                error  $ thing ++ " is not well formed."

        Right t'        
         ->     return t'


instance Check   (Tree c)    => CheckIO (Tree c) where
 check = checkIO "Tree"

instance Check   (Forest c)  => CheckIO (Forest c) where
 check = checkIO "Forest"

instance Check   (Key c)     => CheckIO (Key c) where
 check = checkIO "Key"

instance Check   (Element c) => CheckIO (Element c) where
 check = checkIO "Element"

