
module Datum.Script.Core.Exp.Prim.PrimType
        ( PrimType (..)
        , namesOfPrimTypes
        , primTypesOfNames)
where
import qualified Datum.Data.Tree.Exp                    as T


-- Primitive types.
data PrimType
        = PTValue                       -- ^ Super type of values.
        | PTRef                         -- ^ Reference to a value in the store.
        | PTName                        -- ^ Name type.
        | PTNum                         -- ^ Supertype of number types.
        | PTTreePath                    -- ^ Datum tree path type.
        | PTFilePath                    -- ^ File path type.
        | PTArray                       -- ^ Array type constructor.
        | PTRecord                      -- ^ Record type constructor.
        | PTTree                        -- ^ Datum tree type.
        | PTForest                      -- ^ Datum forest type.
        | PTAtom     !T.AtomType        -- ^ Atom types.
        deriving Show
        

-- | Table of names of primitive types.
namesOfPrimTypes :: [(PrimType, String)]
namesOfPrimTypes 
 =      [ (PTValue,             "Value")
        , (PTRef,               "Ref")
        , (PTName,              "Name")
        , (PTNum,               "Num")
        , (PTTreePath,          "TreePath")
        , (PTFilePath,          "FilePath")
        , (PTArray,             "Array")
        , (PTRecord,            "Record")
        , (PTTree,              "Tree")
        , (PTForest,            "Forest")
        , (PTAtom T.ATUnit,     "Unit")
        , (PTAtom T.ATBool,     "Bool")
        , (PTAtom T.ATInt,      "Int")
        , (PTAtom T.ATFloat,    "Float")
        , (PTAtom T.ATNat,      "Nat")
        , (PTAtom T.ATDecimal,  "Decimal")
        , (PTAtom T.ATText,     "Text")
        , (PTAtom T.ATTime,     "Time") 
        , (PTAtom T.ATDate,     "Date") ]


-- | Table of primitive types of names.
primTypesOfNames :: [(String, PrimType)]
primTypesOfNames
 = [ (name, op) | (op, name) <- namesOfPrimTypes ]

