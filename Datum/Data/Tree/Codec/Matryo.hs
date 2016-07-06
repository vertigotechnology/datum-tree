
module Datum.Data.Tree.Codec.Matryo
        (encodeMatryo)
where
import Datum.Data.Tree.Exp
import Data.Monoid
import Data.ByteString.Builder                  (Builder)
import qualified Data.ByteString.Builder        as BB
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified Data.List                      as List
import qualified Data.Repa.Array                as A


-- | Encode a tree to a lazy `ByteString` in Matryoshka format.
encodeMatryo :: Tree 'O -> BS8.ByteString
encodeMatryo tree'
 = BB.toLazyByteString $ encodeTree 0 tree'
 where


-- | Encode a tree.
encodeTree :: Int -> Tree 'O -> Builder
encodeTree   i (Tree b bt)
        =  encodeBranchType i True bt
        <> encodeBranch     i True b


-- | Encode a branch type. 
--   We track the current indent and whether this is the
--   first branch type in the tree meta-data.
encodeBranchType :: Int -> Bool -> BranchType -> Builder
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


-- | Encode a list of branch types.
encodeBranchTypes :: Int -> [BranchType] -> Builder
encodeBranchTypes _i []
        = mempty

encodeBranchTypes i0 bts
 =   BB.string8 (replicate i0 ' ') <> BB.string8 "[ "
 <> (encodeBranchTypess i0 (0 :: Int) bts)
 <>  BB.string8 (replicate i0 ' ') <> BB.string8 "]"

 where  encodeBranchTypess i 0 (b : bs)
         =  encodeBranchType i False b
         <> encodeBranchTypess i 1 bs

        encodeBranchTypess i n (b : bs)
         =  BB.string8 (replicate i ' ')
         <> BB.string8 ", " <> encodeBranchType i False b
         <> encodeBranchTypess i (n + 1) bs

        encodeBranchTypess _ _ []
         = mempty


-- | Encode a tuple type.
encodeTupleType :: TupleType -> Builder
encodeTupleType (TT kts)
        =  BB.char8 '('
        <> ( mconcat $ List.intersperse (BB.string8 ", ")
           $ map encodeKeyType $ A.toList kts)
        <> BB.char8 ')'


-- | Encode a key type.
encodeKeyType :: (Box Name :*: Box AtomType) -> Builder
encodeKeyType (Box n :*: Box at)
        =  BB.string8 (show n)
        <> BB.string8 ": "
        <> encodeAtomType at


-- | Encode a branch. 
--   We track the current indent and whether this is the
--   first branch type in the tree.
encodeBranch :: Int -> Bool -> Branch -> Builder
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


-- | Encode a branch group.
encodeGroup :: Int -> Group -> Builder
encodeGroup  i0 (G _name bs0)
 =   BB.string8 (replicate i0 ' ') <> BB.string8 "[ "
 <> (encodeGroupBranches   i0 0 $ unboxes bs0)
 <>  BB.string8 (replicate i0 ' ') <> BB.string8 "]"

 where  encodeGroupBranches i (0 :: Int) (b : bs)
         =  encodeBranch i False b 
         <> encodeGroupBranches i 1 bs

        encodeGroupBranches i n (b : bs)
         =  BB.string8 (replicate i ' ') 
         <> BB.string8 ", " <> encodeBranch i False b
         <> encodeGroupBranches i (n + 1) bs

        encodeGroupBranches _ _ []
         = mempty


-- | Encode a tuple.
encodeTuple :: Tuple -> Builder
encodeTuple  (T as)
        =  BB.char8 '('
        <> ( mconcat $ List.intersperse (BB.string8 ", ") 
           $ map encodeAtom $ unboxes as)
        <> BB.char8 ')'


-- | Encode an atom.
encodeAtom :: Atom -> Builder
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


-- | Encode an atom type.
encodeAtomType :: AtomType -> Builder
encodeAtomType at
 = case at of
        ATUnit          -> BB.string8 "()"
        ATBool          -> BB.string8 "Bool"
        ATInt           -> BB.string8 "Int"
        ATFloat         -> BB.string8 "Float"
        ATNat           -> BB.string8 "Nat"
        ATDecimal       -> BB.string8 "Decimal"
        ATText          -> BB.string8 "Text"
        ATTime          -> BB.string8 "Time"

