
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


-------------------------------------------------------------------------------
line    = fromString "\n"
pad  i  = fromString (replicate i ' ')
text s  = fromString s
-------------------------------------------------------------------------------


-- | Encode a tree to a lazy `ByteString` in Matryoshka format.
encodeMatryo :: Tree 'O -> Text
encodeMatryo tree'
 = toLazyText $ encodeTree 0 tree'


-- | Encode a tree.
encodeTree :: Int -> Tree 'O -> Builder
encodeTree   i (Tree b bt)
        =  encodeBranchType i True bt
        <> encodeBranch     i True bt b


-- | Encode a branch type. 
--   We track the current indent and whether this is the
--   first branch type in the tree meta-data.
encodeBranchType :: Int -> Bool -> BranchType -> Builder
encodeBranchType i bFirst (BT n tt bts)
        |  [] <- unboxes bts
        =  text (show n) <> text ": "
        <> encodeTupleType tt
        <> text "\n"

        | otherwise
        =  (if i == 0 
                then mempty
                else text (show n) <> line <> pad i <> text ": ")
        <> text "{ " <> encodeTupleType tt <> line
        <> encodeBranchTypes (i + 4) (unboxes bts)
        <> line
        <> (if bFirst
               then mempty
               else text (replicate (i + 2) ' '))
        <> text "}" <> line


-- | Encode a list of branch types.
encodeBranchTypes :: Int -> [BranchType] -> Builder
encodeBranchTypes _i []
        = mempty

encodeBranchTypes i0 bts
 =   text (replicate i0 ' ') <> text "[ "
 <> (encodeBranchTypess i0 (0 :: Int) bts)
 <>  text (replicate i0 ' ') <> text "]"

 where  encodeBranchTypess i 0 (b : bs)
         =  encodeBranchType   i False b
         <> encodeBranchTypess i 1     bs

        encodeBranchTypess i n (b : bs)
         =  text (replicate i ' ')
         <> text ", " <> encodeBranchType i False b
         <> encodeBranchTypess i (n + 1) bs

        encodeBranchTypess _ _ []
         = mempty


-- | Encode a tuple type.
encodeTupleType :: TupleType -> Builder
encodeTupleType (TT kts)
        =  text "("
        <> ( mconcat $ List.intersperse (text ", ")
           $ map encodeKeyType $ A.toList kts)
        <> text ")"


-- | Encode a key type.
encodeKeyType :: (Box Name :*: Box AtomType) -> Builder
encodeKeyType (Box n :*: Box at)
        =  text (show n)
        <> text ": "
        <> encodeAtomType at


-- | Encode a branch. 
--   We track the current indent and whether this is the
--   first branch type in the tree.
encodeBranch :: Int -> Bool -> BranchType -> Branch -> Builder
encodeBranch i bFirst (BT _n _ bts) (B t gs)
        |  [] <- unboxes gs
        =  encodeTuple t <> line

        | otherwise
        =  text "{ " <> encodeTuple t <> line
        <> (mconcat $ zipWith (encodeGroup (i + 4)) (unboxes bts) (unboxes gs))
        <> line
        <> (if bFirst
               then mempty
               else text (replicate (i + 2) ' '))
        <> text "}"  <> line


-- | Encode a branch group.
encodeGroup :: Int -> BranchType -> Group -> Builder
encodeGroup  i0 bt@(BT name tt _) (G _name bs0)
 =  pad  i0   <> text (show name)                <> line
 <> pad  i0   <> text ": " <> encodeTupleType tt <> line
 <> pad  i0   <> text "[ "
 <> (encodeGroupBranches   i0 0 (unboxes bs0))
 <> pad  i0   <> text "]"

 where  encodeGroupBranches i (0 :: Int) (b : bs)
         =  encodeBranch        i False bt  b 
         <> encodeGroupBranches i 1     bs

        encodeGroupBranches i n          (b : bs)
         =  text (replicate i ' ') 
         <> text ", " <> encodeBranch i False bt b
         <> encodeGroupBranches i (n + 1) bs

        encodeGroupBranches _ _ _
         = mempty


-- | Encode a tuple.
encodeTuple :: Tuple -> Builder
encodeTuple  (T as)
        =  text "("
        <> ( mconcat $ List.intersperse (text ", ") 
           $ map encodeAtom $ unboxes as)
        <> text ")"


-- | Encode an atom.
encodeAtom :: Atom -> Builder
encodeAtom a
 = case a of
        AUnit{}         -> text "Unit"
        ABool    b      -> text $ show b
        AInt     i      -> text $ show i
        AFloat   f      -> text $ show f
        ANat     n      -> text $ show n
        ADecimal n      -> text $ show n
        AText    str    -> text $ show str
        ATime    str    -> text   str


-- | Encode an atom type.
encodeAtomType :: AtomType -> Builder
encodeAtomType at
 = case at of
        ATUnit          -> text "Unit"
        ATBool          -> text "Bool"
        ATInt           -> text "Int"
        ATFloat         -> text "Float"
        ATNat           -> text "Nat"
        ATDecimal       -> text "Decimal"
        ATText          -> text "Text"
        ATTime          -> text "Time"

