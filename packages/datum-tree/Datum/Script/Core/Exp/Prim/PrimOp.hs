
module Datum.Script.Core.Exp.Prim.PrimOp
        ( PrimOp (..)
        , namesOfPrimOps
        , primOpsOfNames
        , arityOfPrimOp)
where


-- Primitive operators.
data PrimOp
        = PPNeg                 -- ^ Negation.
        | PPAdd                 -- ^ Addition.
        | PPSub                 -- ^ Subtraction.
        | PPMul                 -- ^ Multiplication.
        | PPDiv                 -- ^ Division
        | PPEq                  -- ^ Equality.
        | PPGt                  -- ^ Greater-than.
        | PPGe                  -- ^ Greater-than-equal.
        | PPLt                  -- ^ Less-than.
        | PPLe                  -- ^ Less-than-equal.

        | PPAppend              -- ^ Append two trees or forests.
        | PPAt                  -- ^ Apply a per-tree function at the given path.
        | PPArgument            -- ^ Get the value of a script argument.
        | PPConcat              -- ^ Concatenate a list of trees or forests.
        | PPFinal               -- ^ Select the final n branches of each subtree.
        | PPFlatten             -- ^ Flatten branches.
        | PPGather              -- ^ Gather branches of a tree into sub trees.
        | PPGroup               -- ^ Group branches by given key field.
        | PPInitial             -- ^ Select the initial n branches of each subtree.
        | PPLoad                -- ^ Load  a value from the file system.
        | PPOn                  -- ^ Apply a per-forest function at the given path. 
        | PPPermuteFields       -- ^ Permute fields of a key.
        | PPPrint               -- ^ Print an object to the console.
        | PPRenameFields        -- ^ Rename fields of key.
        | PPSample              -- ^ Sample n intermediate branches of each subtree.
        | PPSortByField         -- ^ Sort trees in a forest.
        | PPStore               -- ^ Store a value to the file system.
        deriving (Eq, Show)


-- | Table of names of primitive operators.
namesOfPrimOps :: [(PrimOp, String)]
namesOfPrimOps
 =      [ (PPAppend,            "append#")
        , (PPNeg,               "neg#")
        , (PPAdd,               "add#")
        , (PPSub,               "sub#")
        , (PPMul,               "mul#")
        , (PPDiv,               "div#")
        , (PPEq,                "eq#")
        , (PPGt,                "gt#")
        , (PPGe,                "ge#")
        , (PPLt,                "lt#")
        , (PPLe,                "le#")

        , (PPAt,                "at#")
        , (PPArgument,          "argument#")
        , (PPConcat,            "concat#")
        , (PPFinal,             "final#")
        , (PPFlatten,           "flatten#")
        , (PPGather,            "gather#")
        , (PPGroup,             "group#")
        , (PPInitial,           "initial#")
        , (PPLoad,              "load#")
        , (PPOn,                "on#")
        , (PPPermuteFields,     "permute-fields#")
        , (PPPrint,             "print#") 
        , (PPRenameFields,      "rename-fields#")
        , (PPSample,            "sample#")
        , (PPSortByField,       "sortby-field#")
        , (PPStore,             "store#")
        ]


-- | Tables of primitive operators of names.
primOpsOfNames :: [(String, PrimOp)]
primOpsOfNames 
 = [ (name, op) | (op, name) <- namesOfPrimOps]


-- | Yield the arity of a primitive operator.
arityOfPrimOp :: PrimOp -> Int
arityOfPrimOp op
 = case op of
        PPNeg           -> 1
        PPAdd           -> 2
        PPSub           -> 2
        PPMul           -> 2
        PPDiv           -> 2

        PPEq            -> 2
        PPGt            -> 2
        PPGe            -> 2
        PPLt            -> 2
        PPLe            -> 2

        PPAppend        -> 2
        PPAt            -> 3
        PPArgument      -> 1
        PPConcat        -> 1
        PPFinal         -> 2
        PPFlatten       -> 1
        PPGroup         -> 2
        PPGather        -> 2
        PPInitial       -> 2
        PPLoad          -> 1
        PPOn            -> 3
        PPPermuteFields -> 2
        PPPrint         -> 1
        PPRenameFields  -> 2
        PPSortByField   -> 2
        PPStore         -> 2
        PPSample        -> 2
