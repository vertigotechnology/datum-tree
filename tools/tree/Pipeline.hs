
module Pipeline where
import Datum.Script.Core.Exp 
import Data.Text        (Text)


-- | Pipeline stage.
data Stage
        = SLoad         FilePath
        | SStore        FilePath
        | SInitial      Int
        | SFinal        Int
        | SSample       Int
        | SGather       [Text]
        | SGroup        Text
        | SRenameFields [Text]
        | SOn           [Text] Stage
        deriving Show


-- | Convert a pipeline stage definition to a core expression.
expOfStage :: Stage -> Exp
expOfStage ss
 = case ss of
        SLoad    filePath       -> XLoad    @@ XText filePath
        SStore   filePath       -> XStore   @@ XText filePath
        SInitial n              -> XInitial @@ XNat  n
        SFinal   n              -> XFinal   @@ XNat  n
        SSample  n              -> XSample  @@ XNat  n
        SGather  ns             -> XGather       @@ XList XTName (map XName ns)
        SGroup   n              -> XGroup        @@ XName n
        SRenameFields ns        -> XRenameFields @@ XList XTName (map XName ns)
        SOn ns s'               -> XOn           @@ XList XTName (map XName ns) @@ expOfStage s'

