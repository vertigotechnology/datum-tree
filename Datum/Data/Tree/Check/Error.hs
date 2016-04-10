
module Datum.Data.Tree.Check.Error
        ( Error (..)
        , ppError)
where
import Datum.Data.Tree.Exp
import Datum.Data.Tree.SExp.Pretty
import Text.PrettyPrint.Leijen
import Data.Repa.Array                  (Array)


-- | Possible type errors.
data Error
        -- | Number of sub trees does not match number of sub dimensions.
        = ErrorArityDim
        { errorPath             :: Path
        , errorGroup            :: Array (Box Group)
        , errorBranchTypes      :: Array (Box BranchType) }

        -- | Number of fields in tuple does not match tuple type.
        | ErrorArityTuple
        { errorPath             :: Path
        , errorAtoms            :: [Atom]  
        , errorElements         :: Array (Box Name :*: Box AtomType) }

        -- | Sub dimension name clash.
        | ErrorClashSubDim
        { errorPathType         :: PathType
        , errorNames            :: [Name] }

        -- | Field name clash.
        | ErrorClashField
        { errorPathType         :: PathType
        , errorNames            :: [Name] }

        -- | Atomic value does not match associated type.
        | ErrorAtom
        { errorPath             :: Path
        , errorAtom             :: Atom
        , errorAtomType         :: AtomType }
        deriving Show


-- | Pretty print an error message.
ppError :: Error -> Doc

ppError (ErrorArityDim path gs bts)
 = let  nsGroup  = [mn | G  mn _   <- unboxes gs]
        nsBranch = [n  | BT n  _ _ <- unboxes bts]

   in   vcat
        $ [ text "Number of branches does not match number of dimensions."
          , text "  on path      = " <> ppPath path
          , text "  group  names = " <> text (show nsGroup)
          , text "  branch names = " <> text (show nsBranch) ]

ppError _ 
 = error "ppError"
