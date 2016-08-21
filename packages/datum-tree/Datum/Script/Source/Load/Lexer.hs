
module Datum.Script.Source.Load.Lexer
        ( Token (..)
        , tokenize)
where
import Datum.Script.Source.Load.Token
import Control.Monad
import qualified Data.Char      as Char
import qualified Data.List      as List


-- | Tokenize a string.
tokenize :: FilePath -> String -> [Loc Token]
tokenize filePath xx0
 = eat 1 1 xx0
 where 
       eat !_l !_c []
        = []

       eat !l !c !(x : xs)

        -- Increment line counter.
        | '\n' <- x
        = eat (l + 1) 1 xs

        -- Ignore whitespace.
        | Char.isSpace x
        = eat l (c + 1) xs

        -- Fixed length tokens.
        | Just (tok, ss, xs') <- matchFixed (x : xs)
        = wrap tok : eat l (c + length ss) xs'

        -- Literal strings.
        | '\"' <- x
        = let 
                eats n acc ss
                 | '\\' : ss1 <- ss
                 , '"'  : ss2 <- ss1
                 = eats (n + 2) ('"' : acc) ss2

                 | '\\' : ss1 <- ss
                 , 'n'  : ss2 <- ss1
                 = eats (n + 2) ('\n' : acc) ss2

                 | '"'  : ss1 <- ss
                 = wrap (KLitString (reverse acc))
                    : eat l (c + n) ss1

                 | c'   : ss1 <- ss
                 = eats (n + 1) (c' : acc) ss1

                 | otherwise
                 = [wrap (KErrorUnterm ss)]

           in eats 0 [] xs

        -- Literal integers.
        | Char.isDigit x
        = let   restOfNum       = takeWhile Char.isDigit   xs
                restOfString    = drop (length restOfNum)  xs
                digits          = x : restOfNum
                num             = (read digits :: Int)
          in    wrap (KLitInt num) 
                 : eat l (c + length digits) restOfString

        -- Variables must start with an alphabetic charater.
        -- Subsequent characters can be alpha or numeric.
        | isVarStart x
        = let   restOfVar       = takeWhile isVarBody      xs
                restOfString    = drop (length restOfVar)  xs
                name            = x : restOfVar
          in    wrap (KVar name)
                 : eat l (c + length name) restOfString

        -- Symbols start with a tick character,
        -- but the tick is not part of the name.
        | x == '\''
        = let   restOfVar       = takeWhile isSymbolBody   xs
                restOfString    = drop (length restOfVar)  xs
                name            = restOfVar
          in    wrap (KSym name)
                 : eat l (c + 1 + length name) restOfString

        -- Operators
        | isOperatorBody x
        = let   restOfVar       = takeWhile isOperatorBody xs
                restOfString    = drop (length restOfVar)  xs
                name            = x : restOfVar
          in    wrap (KOp name)
                 : eat l (c + length name) restOfString

        -- If we see a junk character then stop scanning.
        | otherwise
        = [wrap (KErrorJunk x)]

        where   wrap t = Loc filePath l c t


-- | Match a fixed length token.
matchFixed  :: String -> Maybe (Token, String, String)
matchFixed str
 = let  match ss (s, t) 
         = case List.stripPrefix s ss of
                Nothing         -> Nothing
                Just ss'        -> Just (t, s, ss')

   in   foldl mplus Nothing
         $ map (match str) tokensFixed


-- | Fixed length tokens.
--   Each of the unicode symbols has a corresponding plain ascii one.
tokensFixed :: [(String, Token)]
tokensFixed
 =      [ ("()",        KUnit)

        , ("(",         KRoundBra)
        , (")",         KRoundKet)

        , ("[",         KSquareBra)
        , ("]",         KSquareKet)

        , ("{",         KBraceBra)
        , ("}",         KBraceKet)

        , ("\\",        KLam),          ("λ",           KLam)
        , ("->",        KRightArrow),   ("→",           KRightArrow)
        , (".",         KDot) 
        , (",",         KComma)
        , (";",         KSemi)
        , ("/",         KSlashForward)
        , ("let",       KKey "let")
        , ("in",        KKey "in")
        , ("if",        KKey "if")
        , ("then",      KKey "then")
        , ("else",      KKey "else") 
        , ("do",        KKey "do")  ]



-- | Character can start a variable name.
isVarStart :: Char -> Bool
isVarStart c 
 = Char.isAlpha c


-- | Character can be in a variable name body.
isVarBody  :: Char -> Bool
isVarBody c
 = Char.isAlphaNum c || c == '-' || c == '_' || c == '#'


-- | Character can be in a symbol name.
isSymbolBody :: Char -> Bool
isSymbolBody c
 = Char.isAlphaNum c || c == '-'  || c == '_'


-- | Character can start an operator.
isOperatorBody :: Char -> Bool
isOperatorBody c
 = elem c 
        [ '!', '@', '$', '%', '^', '&', '*', '?'
        , '-', '+', '=', '>', '<', '|', '~']

