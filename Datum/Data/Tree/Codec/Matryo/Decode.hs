{-# LANGUAGE OverloadedStrings #-}
module Datum.Data.Tree.Codec.Matryo.Decode
        ( decodeTree
        , Error (..)

        -- * Parsing
        , type Parser

        -- ** Trees
        , pTreeRoot
        , pTree

        -- ** Types
        , pBranchTypeRoot
        , pBranchType
        , pTupleType
        , pFieldType
        , pAtomType

        -- ** Values
        , pBranch
        , pGroup
        , pTuple
        , pName
        , pAtom)
where
import Datum.Data.Tree.Codec.Matryo.Lexer
import Datum.Data.Tree.Codec.Matryo.Token
import Data.Functor.Identity
import Text.Parsec                              (SourcePos, (<?>))
import qualified Datum.Data.Tree.Exp            as T
import qualified Text.Lexer.Inchworm.Char       as I
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P
import qualified Data.Repa.Array                as A
import qualified System.IO.Unsafe               as System
import qualified Data.Text                      as Text


type Parser a
        = P.ParsecT [Located Token] () Identity a


-- | Things that can go wrong when loading Matryoshka trees.
data Error
        = ErrorLexical  SourcePos
        | ErrorParse    P.ParseError
        deriving Show


-- | Decode a Matryoshka tree from a `Text` object.
decodeTree
        :: FilePath
        -> Text.Text
        -> Either Error (T.Tree 'T.X)

decodeTree filePath text
 = System.unsafePerformIO
 $ do   result  <- scanMatryo filePath (Text.unpack text)
        case result of
         (toks, (I.Location line col), strLeftover)
          |  not $ null strLeftover
          -> return $ Left (ErrorLexical $ P.newPos filePath line col)

          |  otherwise
          -> case parseMatryo filePath toks of
                Left err        -> return $ Left  (ErrorParse err)
                Right tree      -> return $ Right tree


-- | Run a parser on located tokens.
parseMatryo 
        :: FilePath
        -> [Located Token]
        -> Either P.ParseError (T.Tree 'T.X)

parseMatryo          filePath tokens
 = P.parse pTreeRoot filePath tokens


-- Tree -----------------------------------------------------------------------
-- | Parse a tree, starting from the root.
--   
-- @TreeRoot   ::= BranchTypeRoot Branch@
--
pTreeRoot :: Parser (T.Tree 'T.X)
 = do   bt      <- pBranchTypeRoot
        b       <- pBranch
        return  $  T.Tree b bt
 <?> "a tree"


-- | Parse a `Tree`.
--
-- @Tree       ::= BranchType Branch@
--
pTree :: Parser (T.Tree 'T.X)
pTree
 = do   bt      <- pBranchType
        b       <- pBranch
        return  $  T.Tree b bt
 <?> "a tree"


-- BranchType -----------------------------------------------------------------
-- | For the root branch type, allow the 'root' name to be elided.
--
-- @
-- BranchTypeRoot 
--            ::= (Name \':\')? '{' TupleType BranchTypes '}'
--             |  (Name \':\')?     TupleType
-- @
--
pBranchTypeRoot :: Parser T.BranchType
pBranchTypeRoot
 = do   name    <- P.choice 
                [ do    n       <- pName
                        _       <- pTok KColon
                        return n

                , do    return "root"]

        tt      <- pTupleType
        bts     <- P.choice
                [ do    _       <- pTok KBraceBra
                        bts'    <- P.sepBy pBranchType (pTok KComma)
                        _       <- pTok KBraceKet
                        return bts'

                , do    bt      <- pBranchType
                        return  [bt]

                , return []
                ]

        return  $ T.BT name tt (T.boxes bts)
 <?> "a branch type"


-- | Parse a `BranchType`.
--
-- @
-- BranchType  ::= Name \':\' '{' TupleType BranchTypes '}'
--              |  Name \':\' TupleType
-- @
--
pBranchType :: Parser T.BranchType
pBranchType
 = do   name    <- pName
        _       <- pTok KColon

        tt      <- pTupleType
        bts     <- P.choice
                [ do    _       <- pTok KBraceBra
                        bts'    <- P.sepBy pBranchType (pTok KComma)
                        _       <- pTok KBraceKet
                        return bts'

                , do    bt      <- pBranchType
                        return [bt]

                , do    return []
                ]

        return  $ T.BT name tt (T.boxes bts)
 <?> "a branch type"


{-
-- | Parse a list of `BranchType`.
-- 
-- @
-- BranchTypes ::= '[' BranchType,* ']'
-- @
--
pBranchTypes :: Parser [T.BranchType]
pBranchTypes
 = do   _       <- pTok KSquareBra
        bts     <- P.sepBy pBranchType (pTok KComma)
        _       <- pTok KSquareKet
        return  bts
 <?> "a list of branch types"
-}

-- | Parse a `T.TupleType`.
--
-- @
-- TupleType   ::= '(' FieldType,* ')'
-- @
--
pTupleType :: Parser T.TupleType
pTupleType
 = do   _       <- pTok KRoundBra
        fs      <- P.sepBy pFieldType (pTok KComma)
        _       <- pTok KRoundKet
        return  $  T.TT $ A.fromList 
                        $ [T.Box n T.:*: T.Box tt | (n, tt) <- fs]
 <?> "a tuple type"


-- | Parse a `FieldType`.
--
-- @ 
-- FieldType    ::= Name \':\' AtomType
-- @
--
pFieldType :: Parser (T.Name, T.AtomType)
pFieldType 
 = do   name    <- pName
        _       <- pTok KColon
        at      <- pAtomType 
        return  (name, at) 
 <?> "a field type"


-- | Parse an `T.AtomType`.
pAtomType  :: Parser T.AtomType
pAtomType 
 = (fmap snd $ pTokMaybe 
   $ \t -> case t of   
                KAtomType x     -> return x
                _               -> Nothing)
 <?> "an atom type"


-- | Parse a `T.Branch`.
--
-- @
-- Branch   ::= '{' Tuple Group,* '}'
--           |  Tuple
-- @
--
pBranch    :: Parser T.Branch
pBranch 
 = do   
        tuple   <- pTuple

        groups  <- P.choice
                [ 
                  -- Multiple sub groups.
                  do    _    <- pTok KBraceBra
                        gs   <- P.sepBy pGroup (pTok KComma)
                        _    <- pTok KBraceKet
                        return gs

                  -- A single sub group.
                , do    g   <- pGroup
                        return [g]

                , do   return []
                ]

        return  $ T.B tuple (T.boxes groups)

 <?> "a branch"


-- | Parse a branch `T.Group`.
--
-- @
-- Group    ::= (Name (\':\' TupleType?))? '[' Branch,* ']
-- @
pGroup  :: Parser T.Group
pGroup
 = do   sName   <- P.choice
                [ do    name    <- pName
                        _       <- pTok KColon
                        return  $ T.Some name

                , do    return  $ T.None ]

        -- TODO: we allow a tuple type here, but we don't check it on load.
        _       <- P.choice [fmap Just pTupleType, return Nothing]

        _       <- pTok KSquareBra
        bs      <- P.sepBy pBranch (pTok KComma)
        _       <- pTok KSquareKet
        return  $  T.G sName (T.boxes bs)
 <?> "a branch group"



-- | Parse a `T.Tuple`.
--
-- @
-- Tuple     ::= '(' Atom,* ')'
--            |  Atom
-- @
--
pTuple :: Parser T.Tuple
pTuple 
 = P.choice
 [ do   _       <- pTok KRoundBra
        fs      <- P.sepBy pAtom (pTok KComma)
        _       <- pTok KRoundKet
        return  $  T.T (T.boxes fs)

 , do   a       <- pAtom
        return  $  T.T (T.boxes [a])
 ]
 <?> "a tuple"
 


-- | Parse a dimension `T.Name`.
pName   :: Parser T.Name
pName
 = (fmap snd $ pTokMaybe 
   $ \t -> case t of
                KAtom (T.AText str)     -> Just str
                _                       -> Nothing)
 <?> "a group name"


-- | Parse an `T.Atom`.
pAtom  :: Parser T.Atom
pAtom 
 = ( fmap snd $ pTokMaybe 
   $ \t -> case t of   
                KAtom a -> return a
                _       -> Nothing)
 <?> "an atom"


-- | Parse the given token, returning its source position.
pTok :: Token -> Parser SourcePos
pTok k  = fmap fst
        $ pTokMaybe (\k' -> if k == k' 
                                then Just ()
                                else Nothing)


-- | Parse a token that matches the given predicate.
pTokMaybe :: (Token -> Maybe a) -> Parser (SourcePos, a)
pTokMaybe f
 = P.token 
        (describeToken . locatedBody)
        locatedSourcePos
        (\l -> case f (locatedBody l) of
                Nothing -> Nothing
                Just a  -> Just (locatedSourcePos l, a))

