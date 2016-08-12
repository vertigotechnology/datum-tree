{-# LANGUAGE OverloadedStrings #-}
module Datum.Data.Tree.Codec.Matryo.Decode
        ( parseMatryo
        , scanMatryo
        , Located (..)
        , pTreeRoot
        , pTree)
where
import Datum.Data.Tree.Codec.Matryo.Lexer
import Datum.Data.Tree.Codec.Matryo.Token
import Data.Functor.Identity
import Text.Parsec                              (SourcePos, (<?>))
import qualified Datum.Data.Tree.Exp            as T
import qualified Text.Parsec                    as P
import qualified Data.Repa.Array                as A


type Parser a
        = P.ParsecT [Located Token] () Identity a


-- | Run a parser on located tokens.
parseMatryo 
        :: FilePath
        -> [Located Token]
        -> Either P.ParseError (T.Tree 'T.X)

parseMatryo          filePath tokens
 = P.parse pTreeRoot filePath tokens


-- Tree -----------------------------------------------------------------------
pTreeRoot :: Parser (T.Tree 'T.X)
 = do   bt      <- pBranchTypeRoot
        b       <- pBranch
        return  $  T.Tree b bt
 <?> "a tree"


-- | Parse a `Tree`.
pTree :: Parser (T.Tree 'T.X)
pTree
 = do   bt      <- pBranchType
        b       <- pBranch
        return  $  T.Tree b bt
 <?> "a tree"


-- BranchType -----------------------------------------------------------------
-- | For the root branch type, allow the 'root' name to be elided.
pBranchTypeRoot :: Parser T.BranchType
pBranchTypeRoot
 = do   name    <- P.choice 
                [ do    n       <- pName
                        _       <- pTok KColon
                        return n

                , do    return "root"]

        _       <- pTok KBraceBra
        tt      <- pTupleType
        bts     <- P.choice [pBranchTypes, return []]
        _       <- pTok KBraceKet
        return  $  T.BT name tt (T.boxes bts)
 <?> "a branch type"


-- | Parse a `BranchType`.
--
--   BRANCHTYPE   ::= NAME ':' '{' TUPLETYPE BRANCHTYPES '}'
--
pBranchType :: Parser T.BranchType
pBranchType
 = do   name    <- pName
        _       <- pTok KColon
        P.choice
         [ do   _       <- pTok KBraceBra
                tt      <- pTupleType
                bts     <- pBranchTypes
                _       <- pTok KBraceKet
                return  $  T.BT name tt (T.boxes bts)

         , do   tt      <- pTupleType
                return  $  T.BT name tt (T.boxes [])
         ]
 <?> "a branch type"


-- | Parse a list of branchtypes.
-- 
---  BRANCHTYPES ::= '[' BRANCHTYPE,+ ']'
--
pBranchTypes :: Parser [T.BranchType]
pBranchTypes
 = do   _       <- pTok KSquareBra
        bts     <- P.sepBy pBranchType (pTok KComma)
        _       <- pTok KSquareKet
        return  bts
 <?> "a list of branch types"


-- | Parse a `TupleType`.
pTupleType :: Parser T.TupleType
pTupleType
 = do   _       <- pTok KRoundBra
        fs      <- P.sepBy pFieldType (pTok KComma)
        _       <- pTok KRoundKet
        return  $  T.TT $ A.fromList 
                        $ [T.Box n T.:*: T.Box tt | (n, tt) <- fs]
 <?> "a tuple type"


-- | Parse a `FieldType`.
pFieldType :: Parser (T.Name, T.AtomType)
pFieldType 
 = do   name    <- pName
        _       <- pTok KColon
        at      <- pAtomType 
        return  (name, at) 
 <?> "a field type"


-- | Parse an `AtomType`.
pAtomType  :: Parser T.AtomType
pAtomType 
 = (fmap snd $ pTokMaybe 
   $ \t -> case t of   
                KAtomType x     -> return x
                _               -> Nothing)
 <?> "an atom type"


-- | Parse a `Branch`.
pBranch    :: Parser T.Branch
pBranch 
 = P.choice
 [ -- Full branch syntax,
   -- with sub groups.
   do   _       <- pTok KBraceBra
        tuple   <- pTuple
        groups  <- P.many pGroup
        _       <- pTok KBraceKet
        return  $  T.B tuple (T.boxes groups)

   -- Simple branch syntax,
   -- if there are no sub groups then we accept just the tuple.
 , do   tuple   <- pTuple
        return  $  T.B tuple (T.boxes [])
 ]
 <?> "a branch"


-- | Parse a branch `Group`.
pGroup  :: Parser T.Group
pGroup
 = do   name    <- pName
        _       <- pTok KColon

        -- TODO: we allow a tuple type here, but we don't check it on load.
        _       <- P.choice [fmap Just pTupleType, return Nothing]
        _       <- pTok KSquareBra
        bs      <- P.sepBy pBranch (pTok KComma)
        _       <- pTok KSquareKet
        return  $  T.G (T.Some name) (T.boxes bs)
 <?> "a branch group"


-- | Parse a dimension `Name`.
pName   :: Parser T.Name
pName
 = (fmap snd $ pTokMaybe 
   $ \t -> case t of
                KAtom (T.AText str)     -> Just str
                _                       -> Nothing)
 <?> "a group name"


-- | Parse a `Tuple`.
pTuple :: Parser T.Tuple
pTuple 
 = do   _       <- pTok KRoundBra
        fs      <- P.sepBy pAtom (pTok KComma)
        _       <- pTok KRoundKet
        return  $  T.T (T.boxes fs)
 <?> "a tuple"
 

-- | Parse an `Atom`.
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

