
module Datum.Data.Tree.Codec.Matryo
        (encodeMatryo)
where
import Datum.Data.Tree.Exp
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified Data.ByteString.Builder        as BB
import qualified Data.List                      as List


---------------------------------------------------------------------------------------------------
-- | Encode a tree to a lazy `ByteString` in Matryoshka format.
encodeMatryo :: Tree 'O -> BS8.ByteString
encodeMatryo tt
 = BB.toLazyByteString $ encodeTree 0 tt
 where
        -------------------------------
        encodeTree   i (Tree b _)
         = encodeBranch i b

        -------------------------------
        encodeGroup  i (G _name bs)
         =   BB.string8 (replicate i ' ') <> BB.string8 "[ "
         <> (encodeGroupBranches   i 0 $ unboxes bs)
         <>  BB.string8 (replicate i ' ') <> BB.string8 "]"

        encodeGroupBranches i (0 :: Int) (b : bs)
         =  encodeBranch i b 
         <> encodeGroupBranches i 1 bs

        encodeGroupBranches i n (b : bs)
         =  BB.string8 (replicate i ' ') 
         <> BB.string8 ", " <> encodeBranch i b
         <> encodeGroupBranches i (n + 1) bs

        encodeGroupBranches _ _ []
         = mempty

        -------------------------------
        encodeBranch _ (B t gs)
         |  [] <- unboxes gs
         =  encodeTuple t
         <> BB.string8 "\n"

        encodeBranch i (B t gs)
         =  BB.string8 "{ "
         <> encodeTuple t
         <> BB.string8 "\n"
         <> (mconcat $ map (encodeGroup (i + 4)) $ unboxes gs)
         <> BB.string8 "\n"
         <> BB.string8 (replicate (i + 2) ' ') 
         <> BB.string8 "}\n"

        -------------------------------
        encodeTuple  (T as)
         =  BB.char8 '('
         <> ( mconcat $ List.intersperse (BB.string8 ", ") 
            $ map encodeAtom $ unboxes as)
         <> BB.char8 ')'

        -------------------------------
        encodeAtom a
         = case a of
                AUnit{}      -> BB.string8  "()"
                ABool    b   -> BB.string8   (show b)
                AInt     i   -> BB.intDec    i
                AFloat   f   -> BB.doubleDec f
                ANat     n   -> BB.intDec    n
                ADecimal n   -> BB.doubleDec n
                AText    str -> BB.string8   $ show str
                ATime    str -> BB.string8   str

