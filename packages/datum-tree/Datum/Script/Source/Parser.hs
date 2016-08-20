
module Datum.Script.Source.Parser where
import Data.Functor.Identity
import Datum.Script.Source.Exp
import Datum.Script.Source.Token                (Token(..), Loc)
import Text.Parsec                              (SourcePos, (<?>))
import qualified Datum.Script.Source.Token      as K
import qualified Datum.Data.Tree.Exp            as T
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P
import qualified Data.Text                      as Text


-------------------------------------------------------------------------------
type Parser a 
        = P.ParsecT [K.Loc K.Token] () Identity a

-- | Run parser on some input tokens.
runParser 
        :: FilePath 
        -> [K.Loc K.Token]
        -> Parser a 
        -> Either P.ParseError a

runParser filePath tokens p 
        = P.parse p filePath tokens


-- | Take the source position of a location.
sourcePosOfLoc :: Loc a -> SourcePos
sourcePosOfLoc loc
        = P.newPos (K.locName loc) (K.locLine loc) (K.locColumn loc)


-- | Yield a source position corresponding to the start of the given file.
startingSourcePos :: FilePath -> SourcePos
startingSourcePos filePath
        = P.newPos filePath 1 1


-------------------------------------------------------------------------------
-- | Parse an entire datum script.
pModule  :: Parser Module
pModule 
 = do   ts              <- fmap (map snd) $ P.many1 pTop
        _               <- pTok KEndOfFile
        return  $ Module 
                { moduleTops = ts }


-------------------------------------------------------------------------------
pTop :: Parser (SourcePos, Top)
pTop 
 = do   (sp, vName)     <- pVar
        vsArgs          <- fmap (map snd) $ P.many pVar
        _               <- pTok (KOp "=")
        (_,  xBody)     <- pExp
        _               <- pTok KSemi
        let vtsArgs     = [(v, Nothing) | v <- vsArgs]
        return  (sp, TBind vName vtsArgs xBody)


-------------------------------------------------------------------------------
-- | Parse an expression.
pExp :: Parser (SourcePos, Exp)
pExp 
 = do   pExpApp


-------------------------------------------------------------------------------
-- | Parse a function application.
pExpApp :: Parser (SourcePos, Exp)
pExpApp
 = do   spx     <- P.many1 pExpAtom
        let (sps, xs) = unzip spx
        let (sp1 : _) = sps
        case xs of
         [x]    -> return (sp1, x)
         _      -> return (sp1, XDefix xs)

 <?> "an expression or application"


-------------------------------------------------------------------------------
pExpAtom :: Parser (SourcePos, Exp)
pExpAtom 
 = P.choice
 [ do   -- parenthesised expression
        _       <- pTok KRoundBra
        spx     <- pExp
        _       <- pTok KRoundKet
        return  spx

 , do   -- lambda abstraction
        sp      <- pTok KLam
        (_,  n) <- pVar
        _       <- pTok KRightArrow
        (_,  x) <- pExp
        return  (sp, XAnnot sp $ XAbs n Nothing x)

 , do   -- do expression
        sp      <- pTok (KKey "do")
        _       <- pTok KBraceBra
        ss      <- P.endBy pStmt (pTok KSemi)
        _       <- pTok KBraceKet

        case reverse ss of
         (SStmt xE : ssFront)
           -> return (sp, XAnnot sp $ XDo (reverse ssFront) xE)

         _ -> fail "malformed do expression"

 , do   -- list
        sp      <- pTok KSquareBra
        xs      <- fmap (map snd) $ P.sepBy1 pExp (pTok KComma)
        _       <- pTok KSquareKet
        return  (sp, XAnnot sp $ XFrag (PVList (XPrim (PHole (XPrim PKData))) xs))

 , do   -- branch path sugar
        sp      <- pTok KSlashForward
        ns      <- fmap (map snd) $ P.sepBy1 pVar (pTok KSlashForward)
        let xs   = [XFrag (PVName n) | n <- Text.pack "root" : ns]
        let hole = XPrim (PHole (XPrim PKData))
        return  (sp, XAnnot sp $ XFrag (PVList hole xs))

 , do   -- variables
        (sp, u) <- pVar
        return  (sp, XAnnot sp $ XVar u) 

 , do   -- infix operators.
        (sp, u) <- pOp
        return  (sp, XAnnot sp $ XInfixOp u) 

 , do   -- symbols
        (sp, s) <- pSymbol
        return  (sp, XAnnot sp $ XFrag (PVName (Text.pack s)))

 , do   -- literal text
        (sp, str) <- pLitString
        return  (sp, XAnnot sp $ XFrag (PVAtom (T.AText str)))

 , do   -- literal integer
        (sp, n)   <- pLitInt
        return  (sp, XAnnot sp $ XFrag (PVAtom (T.AInt n)))

 , do   -- literal unit
        sp      <- pTok KUnit
        return  (sp, XAnnot sp $ XPrim PVUnit)
 ]
 <?> "an atomic expression"


-- | Parse a statement.
pStmt :: Parser Stmt
pStmt 
 = P.choice 
 [ P.try $
    do  (_, v)  <- pVar 
        _       <- pTok (KOp "=")
        (_, x)  <- pExp 
        return  $ SBind v x

 , do   (_, x)  <- pExp
        return  $ SStmt x
 ]


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


-- | Parse an infix operator.
pOp :: Parser (SourcePos, Name)
pOp    = pTokMaybe 
        $ \k -> case k of
                 KOp name       -> Just (Text.pack name)
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

