
module Datum.Data.Tree.Codec.XSV
        ( encodeXSV
        , decodeXSV
        , readXSV

          -- * Headers
        , Csv.HasHeader (..))
where
import Datum.Data.Tree.Exp
import Datum.Data.Tree.Codec.SExp
import Data.Monoid
import Data.Text                                (Text)
import Data.Text.Encoding                       (decodeUtf8)
import qualified Datum.Data.Tree.Codec.Format   as F
import qualified Data.Csv                       as Csv
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as BB
import qualified Data.Vector                    as V
import qualified Data.Repa.Array                as A
import qualified Data.List                      as List
import qualified Data.Char                      as Char
import qualified Data.Text                      as Text
import qualified Data.Repa.Scalar.Date32        as Date32


---------------------------------------------------------------------------------------------------
-- | Encode a tree to CSV in a lazy `ByteString`. 
--   TODO: write header.
encodeXSV :: Char -> Csv.HasHeader -> Tree 'O -> BS8.ByteString
encodeXSV delim _hasHeader tt
 = BB.toLazyByteString $ encodeTree tt
 where
        strDelim
         = [delim]

        encodeTree   (Tree b _)
         = encodeBranch b

        encodeGroup  (G _name bs)
         = mconcat   $ map encodeBranch $ unboxes bs

        encodeBranch (B t gs)
         =   encodeTuple t
         <> (mconcat $ map encodeGroup  $ unboxes gs)

        encodeTuple  (T as)
         = case unboxes as of
                -- The root note of the tree usually has an empty tuple,
                -- which we want to supppress in the output.
                []      -> mempty
                as'     -> ( mconcat 
                           $ List.intersperse (BB.string8 strDelim)
                           $ map encodeAtom as')
                        <> (BB.string8 "\n")

        encodeAtom a
         = case a of
                AUnit{}        -> BB.string8  "()"
                ABool    b     -> BB.string8   (show b)
                AInt     i     -> BB.intDec    i
                AFloat   f     -> BB.doubleDec f
                ANat     n     -> BB.intDec    n
                ADecimal n     -> BB.doubleDec n
                AText    str   -> BB.string8   $ show str
                ATime    str   -> BB.string8   str

                ADate d
                 -> let (dd, mm, yy) = Date32.unpack d
                    in  BB.string8 "d'"
                        <> BB.intDec    yy <> (BB.string8 " ")
                        <> BB.intDec    mm <> (BB.string8 " ")
                        <> BB.intDec    dd


---------------------------------------------------------------------------------------------------
-- | Decode a XSV file from a lazy `ByteString`.
decodeXSV :: Char -> Csv.HasHeader -> BS8.ByteString -> Either String (Tree 'O)
decodeXSV cDelim hasHeader bs
 = do   
        let w8Delim 
             = fromIntegral $ Char.ord cDelim

        let decodeOptions
             = Csv.defaultDecodeOptions
             { Csv.decDelimiter     = w8Delim }

        -- Decode the file into a vector of rows of vectors of fields.
        (lsName, vvb)     
         <- case hasHeader of
                Csv.NoHeader
                 -> do  -- Decode the CSV source data.
                        vvb       <- Csv.decodeWith decodeOptions Csv.NoHeader bs

                        -- Maximum number of fields in any row.
                        let nCols  = V.maximum $ V.map V.length vvb

                        -- Make dummy column names, as we don't have a header
                        -- telling us what they should be.
                        let lsName =  ["col" ++ show n | n <- [0 .. (nCols - 1)]]

                        return  (lsName, vvb)

                Csv.HasHeader
                 -> do  -- Decode the CSV source data, also getting the column
                        -- names from the header. We actually set NoHeader here
                        -- so that we get it as the first row, rather than
                        -- it being skipped.
                        vvb     <- Csv.decodeWith decodeOptions Csv.NoHeader bs

                        if (V.length vvb == 0)
                         then return ([], V.empty)
                         else let -- Unpack the column names.
                                  lsName = map BS8.unpack
                                         $ V.toList $ V.head vvb

                              in  return (lsName, V.tail vvb)

        -- Make a branch for a single row of the file.
        let makeRow vFields
                = let  lFields = map BS8.unpack $ V.toList vFields
                  in   B (T $ boxes $ map AText lFields) A.empty

        return
         $ Tree (branch tuple 
                        (G None $ A.fromList
                                $ map (box . makeRow)
                                $ V.toList vvb))
                (tbranch "root"
                        (ttuple)
                        (tbranch "row" 
                                (TT $ A.fromList 
                                    $ map (flip telement ttext) lsName)))


---------------------------------------------------------------------------------------------------
-- | Decode an XSV file from a lazy `ByteString` and read the fields
--   using the given names and formats.
readXSV :: Char                                 -- ^ Character that separates the column.s
        -> [(Text, (AtomType, F.Format))]       -- ^ Names and formats of desired columns.
        -> BS8.ByteString                       -- ^ Bytestring data to read.
        -> Either String (Tree 'O)

readXSV cDelim ntsFormat bs
 = do
        let w8Delim 
             = fromIntegral $ Char.ord cDelim

        let decodeOptions
             = Csv.defaultDecodeOptions
             { Csv.decDelimiter     = w8Delim }

        -- Decode the file into a vector of rows of vectors of fields.
        -- Decode the CSV source data, also getting the column
        -- names from the header. We actually set NoHeader here
        -- so that we get it as the first row, rather than
        -- it being skipped.
        (  vsName :: V.Vector Text
         , vvb    :: V.Vector (V.Vector BS.ByteString) )
         <- do  vvb     <- Csv.decodeWith decodeOptions Csv.NoHeader bs
                if (V.length vvb == 0)
                  then return (V.empty, V.empty)
                  else let -- Unpack the column names.
                           lsName = V.map decodeUtf8 $ V.head vvb
                       in  return (lsName, V.tail vvb)

        -- Make a branch for a single row of the file.
        let makeField (vsFields :: V.Vector BS.ByteString) (name, (_type, fmt))
                | Just ix       <- lookup name (zip (V.toList vsName) [0..])
                , bsField       <- V.toList vsFields !! ix
                , Just a        <- F.readAtomOfFormat fmt (decodeUtf8 bsField)
                = a

                | otherwise
                = error $ "makeField " ++ show (vsFields, vsName, (name, fmt))

        let makeRow (vsFields :: V.Vector BS.ByteString)
                = let   lsAtom  = map (makeField vsFields) ntsFormat

                  in    B (T $ boxes lsAtom) A.empty

        let tsField = map (fst . snd) ntsFormat

        return
         $ Tree (branch tuple 
                        (G None $ A.fromList
                                $ map (box . makeRow)
                                $ V.toList vvb))
                (tbranch "root"
                        (ttuple)
                        (tbranch "row" 
                                (TT $ A.fromList 
                                    $ zipWith telement
                                        (map Text.unpack $ V.toList vsName)
                                        tsField)))




