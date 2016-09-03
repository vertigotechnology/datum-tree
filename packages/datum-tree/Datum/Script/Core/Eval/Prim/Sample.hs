
module Datum.Script.Core.Eval.Prim.Sample
        (step_Sample)
where
import Datum.Script.Core.Eval.Prim.Base
import qualified Datum.Data.Tree        as T


-- Take the initial n branches of each subtree.
step_Sample _ _ PPInitial [vn, VTree   tree]
 | Just n  <- takeVNat vn
 =      progress $ VTree   $ T.initial n tree

step_Sample _ _ PPInitial [vn, VForest forest]
 | Just n  <- takeVNat vn
 =      progress $ VForest $ T.initial n forest


-- Take the final n branches of each subtree.
step_Sample _ _ PPFinal   [vn, VTree   tree]
 | Just n  <- takeVNat vn
 =      progress $ VTree   $ T.final n tree

step_Sample _ _ PPFinal   [vn, VForest forest]
 | Just n  <- takeVNat vn
 =      progress $ VForest $ T.final n forest


-- Sample n intermediate branches of each subtree.
step_Sample _ _ PPSample  [vn, VTree   tree]
 | Just n  <- takeVNat vn
 =      progress $ VTree   $ T.sample n tree

step_Sample _ _ PPSample  [vn, VForest forest]
 | Just n  <- takeVNat vn
 =      progress $ VForest $ T.sample n forest

-- Unhandled
step_Sample _ state _ _
 =      crash state
