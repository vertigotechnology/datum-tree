
module Datum.Script.Source.Parser where
import Data.Functor.Identity
import Datum.Script.Exp.Core
import Datum.Script.Exp.Prim
import Datum.Script.Source.Token                (Token(..), Loc)
import Text.Parsec                              (SourcePos)
import qualified Datum.Script.Source.Lexer      as Lexer
import qualified Datum.Script.Source.Token      as K
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P
import qualified Data.Text                      as Text


---------------------------------------------------------------------------------------------------
type Parser a 
        = P.ParsecT [K.Loc K.Token] () Identity a


-- | Take the source position of a location.
sourcePosOfLoc :: Loc a -> SourcePos
sourcePosOfLoc loc
        = P.newPos (K.locName loc) (K.locLine loc) (K.locColumn loc)


-- | Run parser on some input tokens.
runParser 
        :: FilePath 
        -> [K.Loc K.Token]
        -> Parser a 
        -> Either P.ParseError a

runParser filePath tokens p 
        = P.parse p filePath tokens


-- | Lex and parse an parse an input string.
loadExp :: FilePath -> String -> Either P.ParseError ExpS
loadExp filePath str
 = fmap snd $ runParser filePath (Lexer.tokenize filePath str) pExpAtom


---------------------------------------------------------------------------------------------------
data Source     = Source
type ExpS       = GExp  Source
type PrimS      = GPrim Source

type instance GXAnnot Source    = SourcePos
type instance GXPrim  Source    = Prim
type instance GXBind  Source    = Bind
type instance GXBound Source    = Bound
type instance GXCast  Source    = Cast


---------------------------------------------------------------------------------------------------
pExpAtom :: Parser (SourcePos, ExpS)
pExpAtom 
 = P.choice
 [ do   -- variables
        (sp, u)   <- pVar
        return  (sp, XAnnot sp $ XVar u) 

 , do   -- symbols
        (sp, s)   <- pSymbol
        return  (sp, XAnnot sp $ XName (Text.pack s))

 , do   -- literal text
        (sp, str) <- pLitString
        return  (sp, XAnnot sp $ XText str)

 , do   -- literal integer
        (sp, n)   <- pLitInt
        return  (sp, XAnnot sp $ XInt n)
 ]


---------------------------------------------------------------------------------------------------
-- | Parse the given keyword.
pKey :: String -> Parser SourcePos
pKey str   
        = fmap fst $ pTokMaybe 
        $ \k -> case k of
                 KKey name      
                  | name == str -> Just (KKey name)
                 _              -> Nothing


-- | Parse a named variable.
pVar :: Parser (SourcePos, Bound)
pVar    = pTokMaybe 
        $ \k -> case k of
                 KVar name      -> Just (UName (Text.pack name))
                 _              -> Nothing


-- | Parse a symbol.
pSymbol    :: Parser (SourcePos, String)
pSymbol = pTokMaybe
        $ \k -> case k of
                 KSym s         -> Just s
                 _              -> Nothing


-- | Parse a literal string.
pLitString :: Parser (SourcePos, String)
pLitString 
        = pTokMaybe 
        $ \k -> case k of
                 KLitString s   -> Just s
                 _              -> Nothing


-- | Parse a literal integer.
pLitInt :: Parser (SourcePos, Int)
pLitInt = pTokMaybe 
        $ \k -> case k of
                 KLitInt s      -> Just s
                 _              -> Nothing


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
        (K.sayToken . K.locBody)
        sourcePosOfLoc
        (\l -> case f (K.locBody l) of
                Nothing -> Nothing
                Just a  -> Just (sourcePosOfLoc l, a))
