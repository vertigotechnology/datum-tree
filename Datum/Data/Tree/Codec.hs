
module Datum.Data.Tree.Codec
        (decodeCSV)
where
import Datum.Data.Tree.Exp
import qualified Data.Csv                       as Csv
import qualified Data.ByteString.Lazy.Char8     as BS8
import qualified Data.Vector                    as V


decodeCSV :: BS8.ByteString -> Either String (Tree 'O)
decodeCSV bs
 = do   
        vvb     <- Csv.decode Csv.NoHeader bs

        let makeRow vfs
                = let  fs      = map BS8.unpack $ V.toList vfs
                  in   B (T $ map AText fs) []

        let g   = G Nothing $ map makeRow 
                $ V.toList vvb

        let len = V.maximum $ V.map V.length vvb

        let bt  = BT    "row" 
                        (TT [("col" ++ show n, ATText) | n <- [0.. len - 1]])
                        []

        return  $ Tree  (B (T []) [g])

                        (BT "root"
                                (TT [("name", ATText)])
                                [bt])



