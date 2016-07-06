
module Datum.Data.Tree.Codec.Matryo
        (encodeMatryo)
where
import Datum.Data.Tree.Exp
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified Data.ByteString.Builder        as BB
import qualified Data.List                      as List
import qualified Data.Repa.Array                as A

---------------------------------------------------------------------------------------------------
-- | Encode a tree to a lazy `ByteString` in Matryoshka format.
encodeMatryo :: Tree 'O -> BS8.ByteString
encodeMatryo tree'
 = BB.toLazyByteString $ encodeTree 0 tree'
 where
        -------------------------------
        encodeTree   i (Tree b bt)
         =  encodeBranchType i True bt
         <> encodeBranch     i True b

        -------------------------------
        -- Encode a branch type. 
        --  We track the current indent and whether this is the
        --  first branch type in the tree meta-data.
        encodeBranchType i bFirst (BT _n tt bts)
         |  [] <- unboxes bts
         =  encodeTupleType tt
         <> BB.string8 "\n"

         | otherwise
         =  BB.string8 "{ "
         <> encodeTupleType tt
         <> BB.string8 "\n"
         <> encodeBranchTypes (i + 4) (unboxes bts)
         <> BB.string8 "\n"
         <> (if bFirst
                then mempty
                else BB.string8 (replicate (i + 2) ' '))
         <> BB.string8 "}\n"

        -------------------------------
        encodeBranchTypes :: Int -> [BranchType] -> BB.Builder
        encodeBranchTypes _i []
         = mempty

        encodeBranchTypes i bts
         =   BB.string8 (replicate i ' ') <> BB.string8 "[ "
         <> (encodeBranchTypess i (0 :: Int) bts)
         <>  BB.string8 (replicate i ' ') <> BB.string8 "]"

        encodeBranchTypess i 0 (b : bs)
         =  encodeBranchType i False b
         <> encodeBranchTypess i 1 bs

        encodeBranchTypess i n (b : bs)
         =  BB.string8 (replicate i ' ')
         <> BB.string8 ", " <> encodeBranchType i False b
         <> encodeBranchTypess i (n + 1) bs

        encodeBranchTypess _ _ []
         = mempty

        -------------------------------
        encodeTupleType (TT kts)
         =  BB.char8 '('
         <> ( mconcat $ List.intersperse (BB.string8 ", ")
            $ map encodeKeyType $ A.toList kts)
         <> BB.char8 ')'

        encodeKeyType (Box n :*: Box at)
         =  BB.string8 (show n)
         <> BB.string8 ": "
         <> BB.string8 (show at)

        -------------------------------
        -- Encode a branch. 
        --  We track the current indent and whether this is the
        --  first branch type in the tree.
        encodeBranch i bFirst (B t gs)
         |  [] <- unboxes gs
         =  encodeTuple t
         <> BB.string8 "\n"

         | otherwise
         =  BB.string8 "{ "
         <> encodeTuple t
         <> BB.string8 "\n"
         <> (mconcat $ map (encodeGroup (i + 4)) $ unboxes gs)
         <> BB.string8 "\n"
         <> (if bFirst
                then mempty
                else BB.string8 (replicate (i + 2) ' '))
         <> BB.string8 "}\n"

        -------------------------------
        encodeGroup  i (G _name bs)
         =   BB.string8 (replicate i ' ') <> BB.string8 "[ "
         <> (encodeGroupBranches   i 0 $ unboxes bs)
         <>  BB.string8 (replicate i ' ') <> BB.string8 "]"

        encodeGroupBranches i (0 :: Int) (b : bs)
         =  encodeBranch i False b 
         <> encodeGroupBranches i 1 bs

        encodeGroupBranches i n (b : bs)
         =  BB.string8 (replicate i ' ') 
         <> BB.string8 ", " <> encodeBranch i False b
         <> encodeGroupBranches i (n + 1) bs

        encodeGroupBranches _ _ []
         = mempty

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

