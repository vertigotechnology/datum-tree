
module Datum.Script.Source.Load.Token
        ( Token (..)
        , sayToken)
where


-- | A token in lambda expression syntax.
data Token
        = KEndOfFile            -- ^ End of file marker.
        
        | KErrorJunk   Char     -- ^ Some junk or invalid character.
        | KErrorUnterm String   -- ^ Unterminated string.

        | KComment     String   -- ^ Comment string.

        | KRoundBra             -- ^ Open  round parenthesis.
        | KRoundKet             -- ^ Close round parenthesis.

        | KSquareBra            -- ^ Open  round parenthesis.
        | KSquareKet            -- ^ Close round parenthesis.

        | KBraceBra             -- ^ Open braces
        | KBraceKet             -- ^ Close braces

        | KLam                  -- ^ Lambda symbol.
        | KRightArrow           -- ^ Right arrow.

        | KDot                  -- ^ Dot character.
        | KComma                -- ^ Comma charater.
        | KSemi                 -- ^ Semicolon.
        | KSlashForward         -- ^ Forward slash.
        | KUnit                 -- ^ The unit value '()'

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
        KEndOfFile      -> "end of file"
        KErrorJunk   _  -> "character"
        KErrorUnterm _  -> "unterminated string"

        KComment _      -> "comment"

        KRoundBra       -> "open round parenthesis"
        KRoundKet       -> "close round parenthesis"

        KSquareBra      -> "open square parenthesis"
        KSquareKet      -> "close square parenthesis"

        KBraceBra       -> "open braces"
        KBraceKet       -> "close braces"

        KLam            -> "lambda"
        KRightArrow     -> "right arrow"

        KDot            -> "dot"
        KComma          -> "comma"
        KSemi           -> "semicolon"
        KSlashForward   -> "forward slash"
        KUnit           -> "unit value"

        KKey k          -> "keyword '"  ++ k ++ "'"
        KVar v          -> "variable '" ++ v ++ "'"
        KOp  o          -> "operator '" ++ o ++ "'"
        KSym s          -> "symbol " ++ s
        KLitString s    -> "literal string "   ++ show s 
        KLitInt i       -> "literal integer '" ++ show i ++ "'"

