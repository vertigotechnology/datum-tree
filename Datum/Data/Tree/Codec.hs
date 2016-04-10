
module Datum.Data.Tree.Codec
        (decodeCSV)
where
import Datum.Data.Tree.Exp
import Datum.Data.Tree.SExp
import qualified Data.Csv                       as Csv
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified Data.Vector                    as V
import qualified Data.Repa.Array                as A


-- | Decode a CSV file from a lazy `ByteString`
decodeCSV :: BS8.ByteString -> Either String (Tree 'O)
decodeCSV bs
 = do   
        -- Decode the file into a vector of rows of vectors of fields.
        -- TODO: slurp header in post processing.
        vvb     <- Csv.decode Csv.NoHeader bs

        -- Make a branch for a single row of the file.
        let makeRow vFields
                = let  lFields = map BS8.unpack $ V.toList vFields
                  in   B (T $ boxes $ map AText lFields) A.empty

        -- Maximum number of fields in any row.
        let len = V.maximum $ V.map V.length vvb

        return
         $ Tree (branch tuple 
                        (G None $ A.fromList
                                $ map (box . makeRow)
                                $ V.toList vvb))
                (tbranch "root"
                        (ttuple)
                        (tbranch "row" 
                                (TT $ A.fromList [ telement ("col" ++ show n) ttext 
                                        | n <- [0.. len - 1]])))
