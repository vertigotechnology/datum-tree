
module Datum.Data.Tree.Codec.XSV
        ( -- * CSV
          encodeCSV
        , decodeCSV

          -- * TSV
        , encodeTSV
        , decodeTSV

          -- * Headers
        , Csv.HasHeader (..))
where
import Datum.Data.Tree.Exp
import Datum.Data.Tree.Codec.SExp
import Data.Monoid
import qualified Data.Csv                       as Csv
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified Data.ByteString.Builder        as BB
import qualified Data.Vector                    as V
import qualified Data.Repa.Array                as A
import qualified Data.List                      as List
import qualified Data.Char                      as Char


---------------------------------------------------------------------------------------------------
-- | Encode a tree to a `CSV` file represented as a lazy `ByteString`.
encodeCSV :: Csv.HasHeader -> Tree 'O -> BS8.ByteString
encodeCSV hasHeader tt
 = encodeXSV ',' hasHeader tt


-- | Decode a tree from a `CSV` file represented as a lazy `ByteString`.
decodeCSV :: Csv.HasHeader -> BS8.ByteString -> Either String (Tree 'O)
decodeCSV hasHeader bs
 = decodeXSV ',' hasHeader bs


-- | Encode a tree to a `CSV` file represented as a lazy `ByteString`.
encodeTSV :: Csv.HasHeader -> Tree 'O -> BS8.ByteString
encodeTSV hasHeader tt
 = encodeXSV '\t' hasHeader tt


-- | Decode a tree from a `CSV` file represented as a lazy `ByteString`.
decodeTSV :: Csv.HasHeader -> BS8.ByteString -> Either String (Tree 'O)
decodeTSV hasHeader bs
 = decodeXSV '\t' hasHeader bs


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
                AUnit{}      -> BB.string8  "()"
                ABool    b   -> BB.string8   (show b)
                AInt     i   -> BB.intDec    i
                AFloat   f   -> BB.doubleDec f
                ANat     n   -> BB.intDec    n
                ADecimal n   -> BB.doubleDec n
                AText    str -> BB.string8   $ show str
                ATime    str -> BB.string8   str


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

