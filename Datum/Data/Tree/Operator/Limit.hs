
module Datum.Data.Tree.Operator.Limit
        ( Initial (..)
        , Final   (..)
        , Sample  (..))
where
import Datum.Data.Tree.Exp
import qualified Data.Repa.Array        as A


------------------------
class Initial a where
 -- | Take only the initital 'n' trees of every Forest.
 initial :: Int -> a -> a

instance Initial (Tree c) where
 initial n (Tree b bt)
  = Tree (initial n b) bt

instance Initial (Forest c) where
 initial n (Forest g bt)
  = Forest (initial n g) bt

instance Initial Branch where
 initial n (B t gs)
  = B t $ A.map (fmap (initial n)) gs

instance Initial Group where
 initial n (G name bs)
  = let iStart   = 0
        iEnd     = max n (A.length bs)
        Just bs' = A.slice iStart iEnd bs
    in  G name  $ A.map (box . initial n . unbox) bs'


------------------------
class Final a where
 -- | Take only the final 'n' trees of every Forest.
 final :: Int -> a -> a

instance Final (Tree c) where
 final n (Tree b bt)
  = Tree (final n b) bt

instance Final (Forest c) where
 final n (Forest g bt)
  = Forest (final n g) bt

instance Final Branch where
 final n (B t gs)
  = B t $ A.map (box . final n . unbox) gs

instance Final Group where
 final n (G name bs)
  = let iStart   = max 0 (A.length bs - n)
        iLen     = max n (A.length bs)
        Just bs' = A.slice iStart iLen bs

    in  G name  $ A.map (box . final n . unbox) bs'


-------------------------
class Sample a where
 -- | Sample each forest so the result contains at most the given
 --   number of trees. The returned trees are spaced regularly, 
 --   for example, to return 5 trees from a 20 tree group, we take
 --   every fourth one.
 sample :: Int -> a -> a

instance Sample (Tree c) where
 sample n (Tree b bt)
  = Tree (sample n b) bt

instance Sample (Forest c) where
 sample n (Forest g bt)
  = Forest (sample n g) bt

instance Sample Branch where
 sample n (B t gs)
  = B t $ A.map (box . sample n . unbox) gs

instance Sample Group where
 sample _ (G name bb)
  | A.length bb == 0
  = G name A.empty

 sample 0 (G name _)
  = G name A.empty

 sample 1 (G name bs)
  | Just bFirst <- A.first bs
  = G name $ A.singleton bFirst

 sample n (G name bb)
  | n >= A.length bb
  = G name bb

  | otherwise
  = let 
        Just bFirst = A.head bb
        Just bLast  = A.last bb
        Just bs     = A.tail bb

        !len    = max 1 (A.length bb)

        !dec    = max 1 (len `div` (max 1 n))

        mids    = boxes
                $ take   (n - 2)
                $ map    snd
                $ filter (\(i, _) -> i `mod` dec == 0)
                $ zip [1..] 
                $ unboxes bs

    in  G name  $ A.map (box . sample n . unbox)
                $ A.concat
                $ A.fromList [A.singleton bFirst, mids, A.singleton bLast]

