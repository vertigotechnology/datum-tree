
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
        SLoad    filePath
         -> XPrimOp PPLoad         @@ XText filePath

        SStore   filePath
         -> XPrimOp PPStore        @@ XText filePath

        SInitial n
         -> XPrimOp PPInitial      @@ XNat  n

        SFinal   n
         -> XPrimOp PPFinal        @@ XNat  n

        SSample  n
         -> XPrimOp PPSample       @@ XNat  n

        SGather  ns
         -> XPrimOp PPGather       @@ XList XTName (map XName ns)

        SGroup   n
         -> XPrimOp PPGroup        @@ XName n

        SRenameFields ns
         -> XPrimOp PPRenameFields @@ XList XTName (map XName ns)

        SOn ns s'
         -> XPrimOp PPOn           @@ XList XTName (map XName ns) @@ expOfStage s'

