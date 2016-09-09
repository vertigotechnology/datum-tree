
module Datum.Script.Core.Eval.Prim.Nesting
        (step_Nesting)
where
import Datum.Script.Core.Eval.Prim.Base
import qualified Datum.Data.Tree                as T
import qualified Datum.Data.Tree.Operator.Cast  as T
import qualified Data.Text                      as Text


-- Gather subtrees.
step_Nesting _ _
        PPGather
        [VArray _ names, VTree tree]
 = do   
        let names' = [ Text.unpack n | PDName n <- names]
        progress $ VTree $ T.gatherTree names' tree


-- Group by a given key.
step_Nesting _ _
        PPGroup
        [VName name, VForest forest]
 =      
        progress $ VForest
                 $ T.promiseForest
                 $ T.groupForest (Text.unpack name) forest

step_Nesting _ _ PPFlatten      [VTree tree]
 = do   progress $ VTree $ T.flattenTree tree


-- Rename a dimension in the tree.
step_Nesting _ _ 
        PPRenameDimension
        [VArray _ names1, VArray _ names2, VTree tree]
 | names1' <- [Text.unpack n | PDName n <- names1]
 , names2' <- [Text.unpack n | PDName n <- names2]
 , length names1' == length names1
 , length names2' == length names2
 = do   
        progress $ VTree 
                 $ T.renameDimOfTree names1' names2' tree


-- Duplicate a dimension in the tree.
step_Nesting _ _
        PPDupDim
        [VName nDimSrc, VName nDimDst, VForest forest]
 = 
        progress $ VForest
                 $ T.dupDimOfForest 
                        (Text.unpack nDimSrc)
                        (Text.unpack nDimDst)
                        forest

step_Nesting _ state _ _
 =      crash  state
