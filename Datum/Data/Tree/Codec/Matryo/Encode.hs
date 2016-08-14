
module Datum.Data.Tree.Codec.Matryo.Encode
        ( prettyTree
        , encodeTree
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
data Config
        = Config
        { configSuppressRoot            :: Bool
        , configSuppressEmptyGroups     :: Bool }

instance Default Config where
 def    = Config 
        { configSuppressRoot            = True
        , configSuppressEmptyGroups     = True }


-------------------------------------------------------------------------------
-- | Pretty print a tree.
prettyTree :: Tree 'O -> Text
prettyTree tree
 = let  (_, b)  = runLayout (layoutTree def tree) 0 1
   in   toLazyText b

-- | Encode a tree as a text builder.
encodeTree :: Config -> Tree 'O -> Builder
encodeTree cc tree
 = let  (_, b)  = runLayout (layoutTree cc tree) 0 1
   in   b


-- | Layout a whole `Tree`.
layoutTree :: Config -> Tree 'O -> Layout
layoutTree cc (Tree b bt)
 =  layoutBranchType cc True bt
 <> line
 <> layoutBranch     cc True bt b


-------------------------------------------------------------------------------
-- | Layout a `BranchType`.
layoutBranchType :: Config -> Bool -> BranchType -> Layout
layoutBranchType cc bRoot (BT n tt bts)
        -- If there are no subdimensions then don't show the empty list.
        | configSuppressEmptyGroups cc 
        , [] <- unboxes bts
        =  (if configSuppressRoot cc && bRoot
                then mempty
                else text (show n) <> text ": ")
        <> layoutTupleType tt

        -- We have a branch type with sub dimensions.
        |  otherwise
        =  (if configSuppressRoot cc && bRoot
                then mempty
                else text (show n) <> line <> text ": ")
        <> layoutTupleType tt
        <> line
        <> indentCollect '{' ',' '}' 4
                (map (layoutBranchType cc False) (unboxes bts))


-- | Layout a tuple type.
layoutTupleType :: TupleType -> Layout
layoutTupleType (TT kts)
        =  text "("
        <> ( mconcat $ List.intersperse (text ", ")
           $ map layoutKeyType $ A.toList kts)
        <> text ")"

-- | Layout a key type.
layoutKeyType :: (Box Name :*: Box AtomType) -> Layout
layoutKeyType (Box n :*: Box at)
        =  text (show n)
        <> text ": "
        <> layoutAtomType at


-------------------------------------------------------------------------------
-- | Layout a branch.
layoutBranch :: Config -> Bool -> BranchType -> Branch -> Layout
layoutBranch cc _bFirst (BT _n _tt bts0) (B t gs0)
 |  A.length gs0 == 0
 =  layoutTuple t

 |  (bt : []) <- unboxes bts0
 ,  (g  : []) <- unboxes gs0
 =  layoutTuple t <> line
 <> indent 2
        (layoutGroup cc bt g)

 |  otherwise
 =  layoutTuple t <> line
 <> indentCollect '{' ',' '}' 2
        (zipWith (layoutGroup cc) (unboxes bts0) (unboxes gs0))


-- | Layout a group.
layoutGroup  :: Config -> BranchType -> Group -> Layout
layoutGroup cc bt (G _name2 bs)
 = indentCollect '[' ',' ']'  2
        (map (layoutBranch cc False bt) (unboxes bs))


-------------------------------------------------------------------------------
-- | Layout a `Tuple`.
layoutTuple :: Tuple -> Layout
layoutTuple  (T as)
 -- If there is a single atom then just print that, without the parens.
 | [a]  <- unboxes as
 = layoutAtom a

 -- Print a full tuple in parens.
 | otherwise
 =  text "("
        <> ( mconcat $ List.intersperse (text ", ") 
           $ map layoutAtom $ unboxes as)
 <> text ")"


-- | Layout an `Atom`.
layoutAtom :: Atom -> Layout
layoutAtom a
 = case a of
        AUnit{}         -> text "Unit"
        ABool    b      -> text $ show b
        AInt     i      -> text $ show i
        AFloat   f      -> text $ show f
        ANat     n      -> text $ show n
        ADecimal n      -> text $ show n
        AText    str    -> text $ show str
        ATime    str    -> text   str


-- | Layout an `AtomType`.
layoutAtomType :: AtomType -> Layout
layoutAtomType at
 = case at of
        ATUnit          -> text "Unit"
        ATBool          -> text "Bool"
        ATInt           -> text "Int"
        ATFloat         -> text "Float"
        ATNat           -> text "Nat"
        ATDecimal       -> text "Decimal"
        ATText          -> text "Text"
        ATTime          -> text "Time"


-------------------------------------------------------------------------------
-- | Layout computation handles indentation.
data Layout
        = Layout
        { runLayout 
                :: Int                  -- Current indentation position.
                -> Int                  -- Current column.
                -> (Int, Builder)       -- Result column, and text builder.
        }

-- | Insert a new line.
line   :: Layout
line
 =  Layout $ \i _
 -> ( i
    , fromString "\n" <> fromString (replicate i ' '))


-- | Insert some text.
text   :: String -> Layout
text tx
 =  Layout $ \_i p
 -> ( p + length tx
    , fromString tx)


-- | Insert an indented sub layout.
indent :: Int -> Layout -> Layout
indent n (Layout l1)
 =  Layout $ \i p
 -> let i' = i + n
    in  if i' > p
         then let (p', b) = l1 i' i'
              in  (p', fromString (replicate (i' - p) ' ') <> b)
         else l1 (i + n) p

instance Monoid Layout where
 mempty 
  =  Layout $ \_ p
  -> (p, mempty)

 mappend (Layout l1) (Layout l2)
  =  Layout $ \i0 p0
  -> let (p1, b1) = l1 i0 p0 
         (p2, b2) = l2 i0 p1
     in  (p2, b1 <> b2)


-- | Layout a collection.
indentCollect :: Char -> Char -> Char -> Int -> [Layout] -> Layout
indentCollect cStart cSep cEnd n ls0
 = case ls0 of
        []      -> text [cStart] <> text [cEnd]
        _       -> start ls0

 where  start ls
         = text [cStart] <> first ls

        first []        = end
        first (l : ls)  = indent n l  <> line <> rest ls

        rest  []        = end
        rest  (l : ls)  = text [cSep] <> indent n l <> line <> rest ls

        end             = text [cEnd]
