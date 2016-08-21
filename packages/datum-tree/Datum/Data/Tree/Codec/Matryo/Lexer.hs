{-# LANGUAGE OverloadedStrings #-}
module Datum.Data.Tree.Codec.Matryo.Lexer
        ( Located (..)
        , locatedSourcePos
        , locatedBody
        , scanMatryo) 
where
import Datum.Data.Tree.Codec.Matryo.Token
import Datum.Data.Tree.Exp
import qualified Text.Lexer.Inchworm.Char       as I
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P
import qualified Data.Char                      as Char


-------------------------------------------------------------------------------
-- | A thing with attached location information.
data Located a
        = Located !FilePath !I.Location !a
        deriving Show


-- | Extract a parsec source pos from a `Located` thing.
locatedSourcePos :: Located a -> P.SourcePos
locatedSourcePos (Located path (I.Location line column) _)
 = P.newPos path line column


-- | Strip the location from a located thing.
locatedBody      :: Located a -> a
locatedBody (Located _ _ x) = x


-------------------------------------------------------------------------------
type Scanner a
        = I.Scanner IO I.Location [Char] a

-- | Scan text Matryoska source.
scanMatryo 
        :: FilePath -> String 
        -> IO ([Located Token], I.Location, String)
scanMatryo filePath str
 = I.scanStringIO str (scanner filePath)
{-# NOINLINE scanMatryo #-}


-- | Scanner for matroshka source.
scanner :: FilePath
        -> I.Scanner IO I.Location [Char] (Located Token)

scanner fileName
 = I.skip Char.isSpace
 $ I.alts
        [ fmap  stamp                   $ scanPunctuation
        , fmap (stamp' KAtom)           $ scanAtom
        , fmap (stamp' KAtomType)       $ scanAtomType
        ]
 where  
        stamp   :: (I.Location, a) -> Located a
        stamp  (l, t) 
          = Located fileName l t
        {-# INLINE stamp #-}

        stamp'  :: (a -> b)
                -> (I.Location, a) -> Located b
        stamp' k (l, t) 
          = Located fileName l (k t)
        {-# INLINE stamp' #-}
{-# INLINE scanner #-}


-- Punctuation ----------------------------------------------------------------
-- | Scan a punctuation token.
scanPunctuation :: Scanner (I.Location, Token)
scanPunctuation
 = I.from matchPunctuation
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
scanAtomType :: Scanner (I.Location, AtomType)
scanAtomType 
 = I.munchPred Nothing matchAtomType acceptAtomType
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
scanAtom :: I.Scanner IO I.Location [Char] (I.Location, Atom)
scanAtom 
 = I.alts
        [ fmap (stamp'  AText)                  $ I.scanHaskellString
        , fmap (stamp' (AInt . fromIntegral))   $ I.scanInteger 
        , I.accepts "()"     AUnit
        , I.accepts "True"  (ABool True)
        , I.accepts "False" (ABool True)
        ]
 where
        stamp'  :: (a -> b)
                -> (I.Location, a) -> (I.Location, b)
        stamp' k (l, t) 
          = (l, k t)
{-# INLINE scanAtom #-}


