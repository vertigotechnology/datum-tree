{-# LANGUAGE OverloadedStrings #-}
module Datum.Data.Tree.Codec.Matryo.Decode
        (scanMatryo)
where
import Datum.Data.Tree.Exp
import qualified Text.Lexer.Inchworm.Char       as Inch
import qualified Data.Char                      as Char

-- | A source token.
data Token 
        = KRoundBra     | KRoundKet
        | KSquareBra    | KSquareKet
        | KBraceBra     | KBraceKet
        | KComma        | KColon

        | KLitInt       Integer
        | KLitString    String

        | KAtomType     AtomType 
        | KAtom         Atom
        deriving Show


-- | A thing with attached location information.
data Located a
        = Located FilePath Inch.Location a
        deriving Show

-- Parser ---------------------------------------------------------------------


-- Scanner --------------------------------------------------------------------
type Scanner a
        = Inch.Scanner IO Inch.Location [Char] a

-- | Scan text Matryoska source.
scanMatryo 
        :: FilePath -> String 
        -> IO ([Located Token], Inch.Location, String)
scanMatryo filePath str
 = Inch.scanStringIO str (scanner filePath)
{-# NOINLINE scanMatryo #-}


-- | Scanner for a lispy language.
scanner :: FilePath
        -> Inch.Scanner IO Inch.Location [Char] (Located Token)

scanner fileName
 = Inch.skip Char.isSpace
 $ Inch.alts
        [ fmap  stamp                   $ scanPunctuation
        , fmap (stamp' KAtom)           $ scanAtom
        , fmap (stamp' KAtomType)       $ scanAtomType
        ]
 where  
        stamp   :: (Inch.Location, a) -> Located a
        stamp  (l, t) 
          = Located fileName l t
        {-# INLINE stamp #-}

        stamp'  :: (a -> b)
                -> (Inch.Location, a) -> Located b
        stamp' k (l, t) 
          = Located fileName l (k t)
        {-# INLINE stamp' #-}
{-# INLINE scanner #-}

-- Punctuation ----------------------------------------------------------------
-- | Scan a punctuation token.
scanPunctuation :: Scanner (Inch.Location, Token)
scanPunctuation
 = Inch.from matchPunctuation
 where
        matchPunctuation :: Char -> Maybe Token
        matchPunctuation c
         = case c of
                '('             -> Just KRoundBra
                ')'             -> Just KRoundKet
                '{'             -> Just KBraceBra
                '}'             -> Just KBraceKet
                '['             -> Just KSquareBra
                ']'             -> Just KSquareKet
                ':'             -> Just KColon
                ','             -> Just KComma
                _               -> Nothing
{-# INLINE scanPunctuation #-}


-- AtomType -------------------------------------------------------------------
-- | Scan an `AtomType`.
scanAtomType :: Scanner (Inch.Location, AtomType)
scanAtomType 
 = Inch.munchPred Nothing matchAtomType acceptAtomType
 where
        matchAtomType  :: Int -> Char -> Bool
        matchAtomType ix c
         = if ix == 0
                then Char.isUpper c
                else Char.isAlpha c

        acceptAtomType :: String -> Maybe AtomType
        acceptAtomType str
         = case str of
                "Unit"          -> Just ATUnit
                "Bool"          -> Just ATBool
                "Int"           -> Just ATInt
                "Float"         -> Just ATFloat
                "Nat"           -> Just ATNat
                "Decimal"       -> Just ATDecimal
                "Text"          -> Just ATText
                "Time"          -> Just ATTime
                _               -> Nothing
{-# INLINE scanAtomType #-}


-- Atom -----------------------------------------------------------------------
-- | Scan an `Atom`.
scanAtom :: Inch.Scanner IO Inch.Location [Char] (Inch.Location, Atom)
scanAtom 
 = Inch.alts
        [ fmap (stamp'  AText)                  $ Inch.scanHaskellString
        , fmap (stamp' (AInt . fromIntegral))   $ Inch.scanInteger 
        , Inch.accepts "()"     AUnit
        , Inch.accepts "True"  (ABool True)
        , Inch.accepts "False" (ABool True)
        ]
 where
        stamp'  :: (a -> b)
                -> (Inch.Location, a) -> (Inch.Location, b)
        stamp' k (l, t) 
          = (l, k t)
{-# INLINE scanAtom #-}


