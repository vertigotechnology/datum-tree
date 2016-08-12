
module Datum.Data.Tree.Codec.Matryo.Encode
        ( encodeTree
        , prettyTree
        , Config (..))
where
import Datum.Data.Tree.Exp
import Data.Monoid
import Data.Default
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

data Config
        = Config
        { configSuppressRoot            :: Bool
        , configSuppressEmptyGroups     :: Bool }

instance Default Config where
 def    = Config 
        { configSuppressRoot            = True
        , configSuppressEmptyGroups     = True }


-- | Encode a tree to a lazy `ByteString` in Matryoshka format.
prettyTree :: Tree 'O -> Text
prettyTree tree'
 = toLazyText $ encodeTree def 0 tree'


-- | Encode a tree.
encodeTree :: Config -> Int -> Tree 'O -> Builder
encodeTree   cc i (Tree b bt)
        =  encodeBranchType cc i True bt
        <> encodeBranch     cc i True bt b


-- | Encode a branch type. 
--   We track the current indent and whether this is the
--   first branch type in the tree meta-data.
encodeBranchType :: Config -> Int -> Bool -> BranchType -> Builder
encodeBranchType cc i bFirst (BT n tt bts)
        | configSuppressEmptyGroups cc 
        , [] <- unboxes bts
        =  text (show n) <> text ": "
        <> encodeTupleType tt
        <> text "\n"

        | otherwise
        =  (if configSuppressRoot cc && i == 0
                then mempty
                else text (show n) <> line <> pad i <> text ": ")
        <> text "{ " <> encodeTupleType tt <> line
        <> encodeBranchTypes cc (i + 4) (unboxes bts)
        <> line
        <> (if bFirst
               then mempty
               else text (replicate (i + 2) ' '))
        <> text "}" <> line


-- | Encode a list of branch types.
encodeBranchTypes :: Config -> Int -> [BranchType] -> Builder
encodeBranchTypes _cc _i []
        = mempty

encodeBranchTypes cc i0 bts
 =   text (replicate i0 ' ') <> text "[ "
 <> (encodeBranchTypess i0 (0 :: Int) bts)
 <>  text (replicate i0 ' ') <> text "]"

 where  encodeBranchTypess        i 0 (b : bs)
         =  encodeBranchType   cc i False b
         <> encodeBranchTypess    i 1     bs

        encodeBranchTypess        i n (b : bs)
         =  text (replicate i ' ')
         <> text ", " <> encodeBranchType cc i False b
         <> encodeBranchTypess    i (n + 1) bs

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
encodeBranch :: Config -> Int -> Bool -> BranchType -> Branch -> Builder
encodeBranch cc i bFirst (BT _n _ bts) (B t gs)
        |  configSuppressEmptyGroups cc
        ,  [] <- unboxes gs
        =  encodeTuple t <> line

        | otherwise
        =  text "{ " <> encodeTuple t <> line
        <> (mconcat $ zipWith (encodeGroup cc (i + 4)) (unboxes bts) (unboxes gs))
        <> line
        <> (if bFirst
               then mempty
               else text (replicate (i + 2) ' '))
        <> text "}"  <> line


-- | Encode a branch group.
encodeGroup :: Config -> Int -> BranchType -> Group -> Builder
encodeGroup  cc i0 bt@(BT name tt _) (G _name bs0)
 =  pad  i0   <> text (show name)                <> line
 <> pad  i0   <> text ": " <> encodeTupleType tt <> line
 <> pad  i0   <> text "[ "
 <> (encodeGroupBranches   i0 0 (unboxes bs0))
 <> pad  i0   <> text "]"

 where  encodeGroupBranches i (0 :: Int) (b : bs)
         =  encodeBranch        cc i False bt  b 
         <> encodeGroupBranches    i 1     bs

        encodeGroupBranches i n          (b : bs)
         =  text (replicate i ' ') 
         <> text ", " <> encodeBranch cc i False bt b
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

