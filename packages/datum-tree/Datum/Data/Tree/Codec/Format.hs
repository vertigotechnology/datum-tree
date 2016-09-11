
module Datum.Data.Tree.Codec.Format
        ( Format (..)
        , readFormat
        , atomTypeOfFormat
        , readAtomOfFormat)
where
import Datum.Data.Tree.Exp
import Data.Text                                (Text)
import qualified Data.Text                      as Text
import qualified Data.Repa.Convert              as R


-- | Data format that we can load directly.
data Format
        = FormatBool
        | FormatInt
        | FormatDecimal
        | FormatText
        | FormatYYYYsMMsDD Char
        deriving Show


-- | Read a format specifier.
readFormat :: String -> Maybe Format
readFormat str
 = case str of
        "Bool"          -> Just FormatBool
        "Int"           -> Just FormatInt
        "Decimal"       -> Just FormatDecimal
        "Text"          -> Just FormatText
        "YYYY-MM-DD"    -> Just (FormatYYYYsMMsDD '-')
        _               -> Nothing


-- | Get the default atom type that represents a format.
atomTypeOfFormat :: Format -> Maybe AtomType
atomTypeOfFormat ff
 = case ff of
        FormatBool              -> Just ATBool
        FormatInt               -> Just ATInt
        FormatDecimal           -> Just ATDecimal
        FormatText              -> Just ATText
        FormatYYYYsMMsDD _      -> Just ATDate


-- | Read a text string using the given format.
readAtomOfFormat :: Format -> Text -> Maybe Atom

readAtomOfFormat FormatText ss
 = Just $ AText (Text.unpack ss)

readAtomOfFormat FormatBool ss
 = case Text.unpack ss of
        "True"  -> Just $ ABool True
        "False" -> Just $ ABool False
        _       -> Nothing

readAtomOfFormat FormatInt ss
 = case R.unpackFromString R.IntAsc (Text.unpack ss) of
        Just i  -> Just $ AInt i
        _       -> Nothing

readAtomOfFormat FormatDecimal ss
 = case R.unpackFromString R.DoubleAsc (Text.unpack ss) of
        Just d  -> Just $ ADecimal d
        _       -> Nothing

readAtomOfFormat (FormatYYYYsMMsDD c) ss
 = case R.unpackFromString (R.YYYYsMMsDD c) (Text.unpack ss) of
        Just d  -> Just $ ADate d
        Nothing -> Nothing


