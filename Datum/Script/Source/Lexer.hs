
module Datum.Script.Source.Lexer
        ( Token (..)
        , tokenize)
where
import Datum.Script.Source.Token
import Control.Monad
import qualified Data.Char      as Char
import qualified Data.List      as List


-- | Tokenize a string.
tokenize :: FilePath -> String -> [Loc Token]
tokenize filePath xx0
 = eat 1 1 xx0
 where 
       eat !l !c []
        = []

       eat !l !c !(x : xs)

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

                 | c    : ss1 <- ss
                 = eats (n + 1) (c : acc) ss1

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

        -- Symbols start with a tick character.
        | x == '\''
        = let   restOfVar       = takeWhile isSymbolBody   xs
                restOfString    = drop (length restOfVar)  xs
                name            = x : restOfVar
          in    wrap (KSym name)
                 : eat l (c + length name) restOfString

        -- Operators
        | isOperatorBody x
        = let   restOfVar       = takeWhile isOperatorBody xs
                restOfString    = drop (length restOfVar)  xs
                name            = x : restOfVar
          in    wrap (KSym name)
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
tokensFixed :: [(String, Token)]
tokensFixed
 =      [ ("(",         KBra)
        , (")",         KKet)
        , ("\\",        KLam)
        , ("λ",         KLam)
        , (".",         KDot) 
        , ("let",       KKey "let")
        , ("in",        KKey "in")
        , ("if",        KKey "if")
        , ("then",      KKey "then")
        , ("else",      KKey "else") ]


-- | Character can start a variable name.
isVarStart :: Char -> Bool
isVarStart c 
 = Char.isAlpha c


-- | Character can be in a variable name body.
isVarBody  :: Char -> Bool
isVarBody c
 = Char.isAlphaNum c || c == '-' || c == '_'


-- | Character can be in a symbol name.
isSymbolBody :: Char -> Bool
isSymbolBody c
 = Char.isAlphaNum c || c == '-'  || c == '_'


-- | Character can start an operator.
isOperatorBody :: Char -> Bool
isOperatorBody c
 = elem c 
        [ '!', '@', '$', '%', '^', '&', '*', '?'
        , '-', '+', '=', '>', '<', '|', '/', '~']

