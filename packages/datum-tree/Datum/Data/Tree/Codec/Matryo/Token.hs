
module Datum.Data.Tree.Codec.Matryo.Token
        ( Token (..)
        , describeToken)
where
import Datum.Data.Tree.Exp


-- | A source token.
data Token 
        = KRoundBra
        | KRoundKet
        | KSquareBra
        | KSquareKet
        | KBraceBra
        | KBraceKet
        | KComma
        | KHash        
        | KColon
        | KDoubleColon

        | KLitInt       Integer
        | KLitString    String

        | KAtomType     AtomType 
        | KAtom         Atom
        deriving (Show, Eq)


-- | Describe a token in English, for error messages. 
describeToken :: Token -> String
describeToken kk
 = case kk of
        KRoundBra       -> "'(' character"
        KRoundKet       -> "')' character"
        KSquareBra      -> "'[' character"
        KSquareKet      -> "']' character"
        KBraceBra       -> "'{' character"
        KBraceKet       -> "'}' character"
        KComma          -> "',' character"
        KHash           -> "'#' character"
        KColon          -> "':' character"
        KDoubleColon    -> "'::' symbol"

        KLitInt i       -> "literal integer '" ++ show i ++ "'"
        KLitString s    -> "literal string " ++ show s

        KAtomType at    -> "atom type " ++ (tail $ tail $ show at)
        KAtom a         -> "atom " ++ show a

