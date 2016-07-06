
module Datum.Data.Tree.Codec.Matryo
        ( encodeMatryo
        , encodeTree)
where
import Datum.Data.Tree.Exp
import Data.Monoid
import Data.Text.Lazy                           (Text)
import qualified Data.List                      as List
import qualified Data.Repa.Array                as A

import Data.Text.Lazy.Builder                   
        (Builder, toLazyText, fromString)


-- | Encode a tree to a lazy `ByteString` in Matryoshka format.
encodeMatryo :: Tree 'O -> Text
encodeMatryo tree'
 = toLazyText $ encodeTree 0 tree'


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
        <> fromString "\n"

        | otherwise
        =  fromString "{ "
        <> encodeTupleType tt
        <> fromString "\n"
        <> encodeBranchTypes (i + 4) (unboxes bts)
        <> fromString "\n"
        <> (if bFirst
               then mempty
               else fromString (replicate (i + 2) ' '))
        <> fromString "}\n"


-- | Encode a list of branch types.
encodeBranchTypes :: Int -> [BranchType] -> Builder
encodeBranchTypes _i []
        = mempty

encodeBranchTypes i0 bts
 =   fromString (replicate i0 ' ') <> fromString "[ "
 <> (encodeBranchTypess i0 (0 :: Int) bts)
 <>  fromString (replicate i0 ' ') <> fromString "]"

 where  encodeBranchTypess i 0 (b : bs)
         =  encodeBranchType i False b
         <> encodeBranchTypess i 1 bs

        encodeBranchTypess i n (b : bs)
         =  fromString (replicate i ' ')
         <> fromString ", " <> encodeBranchType i False b
         <> encodeBranchTypess i (n + 1) bs

        encodeBranchTypess _ _ []
         = mempty


-- | Encode a tuple type.
encodeTupleType :: TupleType -> Builder
encodeTupleType (TT kts)
        =  fromString "("
        <> ( mconcat $ List.intersperse (fromString ", ")
           $ map encodeKeyType $ A.toList kts)
        <> fromString ")"


-- | Encode a key type.
encodeKeyType :: (Box Name :*: Box AtomType) -> Builder
encodeKeyType (Box n :*: Box at)
        =  fromString (show n)
        <> fromString ": "
        <> encodeAtomType at


-- | Encode a branch. 
--   We track the current indent and whether this is the
--   first branch type in the tree.
encodeBranch :: Int -> Bool -> Branch -> Builder
encodeBranch i bFirst (B t gs)
        |  [] <- unboxes gs
        =  encodeTuple t
        <> fromString "\n"

        | otherwise
        =  fromString "{ "
        <> encodeTuple t
        <> fromString "\n"
        <> (mconcat $ map (encodeGroup (i + 4)) $ unboxes gs)
        <> fromString "\n"
        <> (if bFirst
               then mempty
               else fromString (replicate (i + 2) ' '))
        <> fromString "}\n"


-- | Encode a branch group.
encodeGroup :: Int -> Group -> Builder
encodeGroup  i0 (G _name bs0)
 =   fromString (replicate i0 ' ') <> fromString "[ "
 <> (encodeGroupBranches   i0 0 $ unboxes bs0)
 <>  fromString (replicate i0 ' ') <> fromString "]"

 where  encodeGroupBranches i (0 :: Int) (b : bs)
         =  encodeBranch i False b 
         <> encodeGroupBranches i 1 bs

        encodeGroupBranches i n (b : bs)
         =  fromString (replicate i ' ') 
         <> fromString ", " <> encodeBranch i False b
         <> encodeGroupBranches i (n + 1) bs

        encodeGroupBranches _ _ []
         = mempty


-- | Encode a tuple.
encodeTuple :: Tuple -> Builder
encodeTuple  (T as)
        =  fromString "("
        <> ( mconcat $ List.intersperse (fromString ", ") 
           $ map encodeAtom $ unboxes as)
        <> fromString ")"


-- | Encode an atom.
encodeAtom :: Atom -> Builder
encodeAtom a
 = case a of
        AUnit{}         -> fromString "Unit"
        ABool    b      -> fromString $ show b
        AInt     i      -> fromString $ show i
        AFloat   f      -> fromString $ show f
        ANat     n      -> fromString $ show n
        ADecimal n      -> fromString $ show n
        AText    str    -> fromString $ show str
        ATime    str    -> fromString   str


-- | Encode an atom type.
encodeAtomType :: AtomType -> Builder
encodeAtomType at
 = case at of
        ATUnit          -> fromString "Unit"
        ATBool          -> fromString "Bool"
        ATInt           -> fromString "Int"
        ATFloat         -> fromString "Float"
        ATNat           -> fromString "Nat"
        ATDecimal       -> fromString "Decimal"
        ATText          -> fromString "Text"
        ATTime          -> fromString "Time"

