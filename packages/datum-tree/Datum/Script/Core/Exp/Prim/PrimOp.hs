
module Datum.Script.Core.Exp.Prim.PrimOp
        ( PrimOp (..)
        , namesOfPrimOps
        , primOpsOfNames
        , arityOfPrimOp)
where


-- Primitive operators.
data PrimOp
        -- Arithmetic operators.
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

        -- Array operators.
        | PPArrayEmpty          -- ^ Construct an empty array.
        | PPArrayExtend         -- ^ Construct a new array with an extra element.

        -- Record operators.
        | PPRecordEmpty         -- ^ Construct an empty record.
        | PPRecordExtend        -- ^ Extend a record with a field of the given name.
        | PPRecordProject       -- ^ Project the field with the given name from a record.

        -- File system operators.
        | PPLoad                -- ^ Load  a value from the file system.
        | PPStore               -- ^ Store a value to the file system.
        | PPRead                -- ^ Read  a value from the file system with a specified format.

        -- Console operators.
        | PPPrint               -- ^ Print an object to the console.

        -- Pure tree operators.
        | PPAppend              -- ^ Append two trees or forests.
        | PPAt                  -- ^ Apply a per-tree function at the given path.
        | PPArgument            -- ^ Get the value of a script argument.
        | PPConcat              -- ^ Concatenate a list of trees or forests.
        | PPCountAsField        -- ^ Count the number of branches in a subtree.
        | PPDropDim             -- ^ Drop a dimension in a tree.
        | PPDupDim              -- ^ Duplicate a dimension in the tree.
        | PPFinal               -- ^ Select the final n branches of each subtree.
        | PPFilterKeys          -- ^ Filter the keys in every branch of a forest.
        | PPFlatten             -- ^ Flatten branches.
        | PPGather              -- ^ Gather branches of a tree into sub trees.
        | PPGroup               -- ^ Group branches by given key field.
        | PPInitial             -- ^ Select the initial n branches of each subtree.
        | PPMapKeys             -- ^ Apply a function to the keys of every branch in a forest.
        | PPOn                  -- ^ Apply a per-forest function at the given path. 
        | PPPermuteFields       -- ^ Permute fields of a key.
        | PPPushDim             -- ^ Push down a dimension.
        | PPRenameFields        -- ^ Rename fields of key.
        | PPRenameDimension     -- ^ Rename a dimension in the tree.
        | PPSample              -- ^ Sample n intermediate branches of each subtree.
        | PPSortByField         -- ^ Sort trees in a forest.
        deriving (Eq, Show)


-- | Table of names of primitive operators.
namesOfPrimOps :: [(PrimOp, String)]
namesOfPrimOps
 =      -- Arithmetic operators.
        [ (PPAppend,            "append#")
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

        -- Array operators.
        , (PPArrayEmpty,        "array-empty#")
        , (PPArrayExtend,       "array-extend#")

        -- Record operators.
        , (PPRecordEmpty,       "record-empty#")
        , (PPRecordExtend,      "record-extend#")
        , (PPRecordProject,     "record-project#")

        -- File system operators.
        , (PPLoad,              "load#")
        , (PPStore,             "store#")
        , (PPRead,              "read#")

        -- Console operators.
        , (PPPrint,             "print#") 

        -- Pure tree operators.
        , (PPAt,                "at#")
        , (PPArgument,          "argument#")
        , (PPConcat,            "concat#")
        , (PPCountAsField,      "count-as-field#")
        , (PPDropDim,           "drop-dim#")
        , (PPDupDim,            "dup-dim#")
        , (PPFinal,             "final#")
        , (PPFilterKeys,        "filter-keys#")
        , (PPFlatten,           "flatten#")
        , (PPGather,            "gather#")
        , (PPGroup,             "group#")
        , (PPInitial,           "initial#")
        , (PPMapKeys,           "map-keys#")
        , (PPOn,                "on#")
        , (PPPermuteFields,     "permute-fields#")
        , (PPPushDim,           "push-dim#")
        , (PPRenameFields,      "rename-fields#")
        , (PPRenameDimension,   "rename-dimension#")
        , (PPSample,            "sample#")
        , (PPSortByField,       "sortby-field#")
        ]


-- | Tables of primitive operators of names.
primOpsOfNames :: [(String, PrimOp)]
primOpsOfNames 
 = [ (name, op) | (op, name) <- namesOfPrimOps]


-- | Yield the arity of a primitive operator.
arityOfPrimOp :: PrimOp -> Int
arityOfPrimOp op
 = case op of
        PPNeg                   -> 1
        PPAdd                   -> 2
        PPSub                   -> 2
        PPMul                   -> 2
        PPDiv                   -> 2
        PPEq                    -> 2
        PPGt                    -> 2
        PPGe                    -> 2
        PPLt                    -> 2
        PPLe                    -> 2

        PPArrayEmpty            -> 0
        PPArrayExtend           -> 2

        PPRecordEmpty           -> 0
        PPRecordExtend          -> 4
        PPRecordProject         -> 2

        PPLoad                  -> 1
        PPStore                 -> 2
        PPRead                  -> 2

        PPAppend                -> 2
        PPAt                    -> 3
        PPArgument              -> 1
        PPConcat                -> 1
        PPCountAsField          -> 3
        PPDropDim               -> 2
        PPDupDim                -> 3
        PPFinal                 -> 2
        PPFilterKeys            -> 2
        PPFlatten               -> 1
        PPGroup                 -> 2
        PPGather                -> 2
        PPInitial               -> 2
        PPMapKeys               -> 2
        PPOn                    -> 3
        PPPermuteFields         -> 2
        PPPushDim               -> 3
        PPPrint                 -> 1
        PPRenameFields          -> 2
        PPRenameDimension       -> 3
        PPSortByField           -> 2
        PPSample                -> 2

