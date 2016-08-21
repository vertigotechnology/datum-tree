
module Datum.Script.Source.Load.Lexer
        ( scanSource
        , Located (..)
        , locatedSourcePos
        , locatedBody)
where
import Datum.Data.Tree.Codec.Matryo.Lexer
        ( Located (..)
        , locatedSourcePos
        , locatedBody)

import Datum.Script.Source.Load.Token
import qualified Text.Lexer.Inchworm.Char       as I
import qualified Data.Char                      as Char


-------------------------------------------------------------------------------
-- | Scan script source.
scanSource
        :: FilePath -> String
        -> IO ([Located Token], I.Location, String)
scanSource filePath str
 = I.scanStringIO str (scanner filePath)
{-# NOINLINE scanSource #-}


-- | Scanner for script soure tokens.
type Scanner a
        = I.Scanner IO I.Location [Char] a


-------------------------------------------------------------------------------
-- | Scanner for script source.
scanner :: FilePath
        -> I.Scanner IO I.Location [Char] (Located Token)

scanner fileName
 = I.skip Char.isSpace
 $ I.alts
        [ fmap (stamp' KComment)
                $ I.scanHaskellCommentBlock

        , fmap (stamp' KComment)
                $ I.scanHaskellCommentLine

        , fmap stamp    $ scanPunctuation
        , fmap stamp    $ scanKeyword
        , fmap stamp    $ scanSymbol
        , fmap stamp    $ scanVar
        , fmap stamp    $ scanOperator 

        , fmap (stamp' KLitString)
                $ I.scanHaskellString 

        , fmap (stamp' (KLitInt . fromIntegral))
                $ I.scanInteger
        ]
 where
        stamp   :: (I.Location, a) -> Located a
        stamp (l, t)
         = Located fileName l t
        {-# INLINE stamp #-}

        stamp'  :: (a -> b)
                -> (I.Location, a) -> Located b
        stamp' k (l, t) 
          = Located fileName l (k t)
        {-# INLINE stamp' #-}


-- Punctuation ----------------------------------------------------------------
-- | Scan a punctuation token.
scanPunctuation :: Scanner (I.Location, Token)
scanPunctuation
 = I.alts     
        [ I.froms (Just 2) acceptPunctuation2 
        , I.from           acceptPunctuation1 ]
 where
        acceptPunctuation1 :: Char -> Maybe Token
        acceptPunctuation1 c
         = case c of
                '('     -> Just KRoundBra
                ')'     -> Just KRoundKet
                '['     -> Just KSquareBra
                ']'     -> Just KSquareKet
                '{'     -> Just KBraceBra
                '}'     -> Just KBraceKet
                '.'     -> Just KDot
                ','     -> Just KComma
                ';'     -> Just KSemi
                '/'     -> Just KSlashForward
                '→'     -> Just KRightArrow
                '\\'    -> Just KLam
                'λ'     -> Just KLam
                _       -> Nothing

        acceptPunctuation2 :: String -> Maybe Token
        acceptPunctuation2 str
         = case str of
                "()"    -> Just KUnit
                "->"    -> Just KRightArrow
                _       -> Nothing
{-# INLINE scanPunctuation #-}


-- Keywords -------------------------------------------------------------------
scanKeyword :: Scanner (I.Location, Token)
scanKeyword
 =      I.munchPred Nothing matchKeyword acceptKeyword
 where
        matchKeyword _ c        = Char.isLower c

        acceptKeyword ss
         = case ss of
                "let"   -> Just $ KKey "let"
                "in"    -> Just $ KKey "in"
                "if"    -> Just $ KKey "if"
                "then"  -> Just $ KKey "then"
                "else"  -> Just $ KKey "else"
                "do"    -> Just $ KKey "do"
                _       -> Nothing


-- Symbols --------------------------------------------------------------------
scanSymbol :: Scanner (I.Location, Token)
scanSymbol
 =      I.munchPred Nothing matchSymbol acceptSymbol
 where
        matchSymbol 0 '\''       = True
        matchSymbol 0  _         = False
        matchSymbol _ c          = isSymbolBody c

        acceptSymbol ('\'' : ss) = Just $ KSym ss
        acceptSymbol _           = Nothing


-- | Character can be in an operator name.
isSymbolBody :: Char -> Bool
isSymbolBody c
 = Char.isAlphaNum c || c == '-'  || c == '_'


-- Variables ------------------------------------------------------------------
scanVar :: Scanner (I.Location, Token)
scanVar
 =      I.munchPred Nothing matchVar acceptVar
 where
        matchVar 0 c    = isVarStart c
        matchVar _ c    = isVarBody  c

        acceptVar ss    = Just $ KVar ss


-- | Character can start a variable name.
isVarStart :: Char -> Bool
isVarStart c 
 = Char.isAlpha c


-- | Character can be in a variable name body.
isVarBody  :: Char -> Bool
isVarBody c
 = Char.isAlphaNum c || c == '-' || c == '_' || c == '#'


-- Operators ------------------------------------------------------------------
scanOperator :: Scanner (I.Location, Token)
scanOperator
 =      I.munchPred Nothing matchOperator acceptOperator
 where  
        matchOperator _ c = isOperatorBody c

        acceptOperator ss = Just $ KOp ss


-- | Character can start an operator.
isOperatorBody :: Char -> Bool
isOperatorBody c
 = elem c 
        [ '!', '@', '$', '%', '^', '&', '*', '?'
        , '-', '+', '=', '>', '<', '|', '~']

