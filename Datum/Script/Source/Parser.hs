
module Datum.Script.Source.Parser where
import Data.Functor.Identity
import Datum.Script.Source.Exp
import Datum.Script.Source.Token                (Token(..), Loc)
import Text.Parsec                              (SourcePos, (<?>))
import qualified Datum.Script.Source.Lexer      as Lexer
import qualified Datum.Script.Source.Token      as K
import qualified Datum.Data.Tree.Exp            as T
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P
import qualified Data.Text                      as Text


-------------------------------------------------------------------------------
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
loadExp :: FilePath -> String -> Either P.ParseError Datum.Script.Source.Exp.Exp
loadExp filePath str
 = let  tokens  =  Lexer.tokenize filePath str 
                ++ [K.Loc filePath 0 0 $ KEndOfFile]
   in   runParser filePath tokens pScript


-------------------------------------------------------------------------------
-- | Parse an entire datum script.
pScript  :: Parser Exp
pScript 
 = do   (_, x)  <- pExp
        _       <- pTok KEndOfFile
        return  x


-------------------------------------------------------------------------------
-- | Parse an expression.
pExp :: Parser (SourcePos, Exp)
pExp 
 = do   pExpApp


-------------------------------------------------------------------------------
-- | Parse a function application.
pExpApp :: Parser (SourcePos, Exp)
pExpApp
 = do   (sp, x1) <- pExpAtom
        
        P.choice
         [ do   -- application of two expressions
                xs      <- P.many1 pExpAtom
                let xApp = foldl (\x (sp', x') -> XAnnot sp' $ XApp x x') x1 xs
                return (sp, xApp)

                -- a single atomic expression
         ,      return (sp, x1)]

 <?> "an expression or application"


-------------------------------------------------------------------------------
pExpAtom :: Parser (SourcePos, Exp)
pExpAtom 
 = P.choice
 [ do   -- parenthesised expression
        _       <- pTok KBra
        spx     <- pExp
        _       <- pTok KKet
        return  spx

 , do   -- variables
        (sp, u) <- pVar
        return  (sp, XAnnot sp $ XVar u) 

 , do   -- symbols
        (sp, s) <- pSymbol
        return  (sp, XAnnot sp $ XPrim (PVName (Text.pack s)))

 , do   -- literal text
        (sp, str) <- pLitString
        return  (sp, XAnnot sp $ XPrim (PVAtom (T.AText str)))

 , do   -- literal integer
        (sp, n)   <- pLitInt
        return  (sp, XAnnot sp $ XPrim (PVAtom (T.AInt n)))
 ]
 <?> "an atomic expression"


-------------------------------------------------------------------------------
-- | Parse the given keyword.
pKey :: String -> Parser SourcePos
pKey str   
        = fmap fst $ pTokMaybe 
        $ \k -> case k of
                 KKey name      
                  | name == str -> Just (KKey name)
                 _              -> Nothing


-- | Parse a named variable.
pVar :: Parser (SourcePos, Name)
pVar    = pTokMaybe 
        $ \k -> case k of
                 KVar name      -> Just (Text.pack name)
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

