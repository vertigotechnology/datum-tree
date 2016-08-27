
module Datum.Script.Source.Load.Located
        ( Located (..)
        , locatedSourcePos
        , locatedBody
        , columnOfLocated)
where
import qualified Text.Lexer.Inchworm.Char       as I
import qualified Text.Parsec.Pos                as P


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


-- | Get the column of a located thing.
columnOfLocated  :: Located a -> Int
columnOfLocated (Located _ (I.Location _ column) _) 
 = column
