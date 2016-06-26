
module Datum.Script.Source.Token
        ( Loc   (..)
        , Token (..)
        , sayToken)
where


-- | A source file location.
data Loc a
        = Loc
        { locLine       :: Int
        , locColumn     :: Int 
        , locBody       :: a }
        deriving Show


-- | A token in lambda expression syntax.
data Token
        = KErrorJunk   Char     -- ^ Some junk or invalid character.
        | KErrorUnterm String   -- ^ Unterminated string.

        | KBra                  -- ^ Open  braket.
        | KKet                  -- ^ Close braket.
        | KLam                  -- ^ Lambda symbol.
        | KDot                  -- ^ Dot symbol.

        | KKey  String          -- ^ Keyword.
        | KVar  String          -- ^ Variable name.
        | KOp   String          -- ^ Operator name.
        | KSym  String          -- ^ Symbol name.

        | KLitString  String    -- ^ Literal string.
        | KLitInt     Int       -- ^ Literal integer.
        deriving (Show, Eq)


-- | Produce an English description of a token.
sayToken :: Token -> String
sayToken tok
 = case tok of
        KErrorJunk   _  -> "unexpected character"
        KErrorUnterm _  -> "unterminated string"
        KBra            -> "open bracket"
        KKet            -> "close bracket"
        KLam            -> "lambda"
        KDot            -> "dot"
        KKey k          -> "keyword '"  ++ k ++ "'"
        KVar v          -> "variable '" ++ v ++ "'"
        KOp  o          -> "operator '" ++ o ++ "'"
        KSym s          -> "symbol " ++ s
        KLitString s    -> "literal string "   ++ show s 
        KLitInt i       -> "literal integer '" ++ show i ++ "'"

