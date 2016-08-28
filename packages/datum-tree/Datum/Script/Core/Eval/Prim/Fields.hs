
module Datum.Script.Core.Eval.Prim.Fields
        (step_Fields)
where
import Datum.Script.Core.Eval.Prim.Base
import qualified Datum.Data.Tree                as T
import qualified Datum.Data.Tree.Operator.Cast  as T
import qualified Data.Text                      as Text


-- Rename fields.
step_Fields _ _ PPRenameFields  [ VList _ names, VTree tree ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VTree
                 $ T.promiseTree
                 $ T.renameFields names' tree

step_Fields _ _ PPRenameFields  [ VList _ names, VForest forest ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VForest
                 $ T.promiseForest
                 $ T.renameFields names' forest


-- Permute fields.
step_Fields _ _ PPPermuteFields [ VList _ names, VTree tree ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VTree
                 $ T.promiseTree
                 $ T.permuteFields names' tree

step_Fields _ _ PPPermuteFields [ VList _ names, VForest forest ]
 = do   let names' = [ Text.unpack n | XName n <- names]
        progress $ VForest
                 $ T.promiseForest
                 $ T.permuteFields names' forest

step_Fields _ _ _ _
 =      crash