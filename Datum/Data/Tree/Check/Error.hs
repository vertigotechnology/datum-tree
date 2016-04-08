
module Datum.Data.Tree.Check.Error
        ( Error (..)
        , ppError)
where
import Datum.Data.Tree.Exp
import Datum.Data.Tree.SExp.Pretty
import Text.PrettyPrint.Leijen


-- | Possible type errors.
data Error
        -- | Number of sub trees does not match number of sub dimensions.
        = ErrorArityDim         Path     [Group] [BranchType] 

        -- | Number of fields in tuple does not match tuple type.
        | ErrorArityTuple       Path     [Atom]  [(Name, AtomType)]

        -- | Sub dimension name clash.
        | ErrorClashSubDim      PathType [Name]

        -- | Field name clash.
        | ErrorClashField       PathType [Name]

        -- | Atomic value does not match associated type.
        | ErrorAtom             Path Atom    AtomType
        deriving Show


-- | Pretty print an error message.
ppError :: Error -> Doc

ppError (ErrorArityDim path gs bts)
 = let  nsGroup  = [mn | G  mn _   <- gs]
        nsBranch = [n  | BT n  _ _ <- bts]

   in   vcat
        $ [ text "Number of branches does not match number of dimensions."
          , text "  on path      = " <> ppPath path
          , text "  group  names = " <> text (show nsGroup)
          , text "  branch names = " <> text (show nsBranch) ]


ppError _ 
 = error "ppError"
