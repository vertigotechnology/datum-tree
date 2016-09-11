
module Datum.Script.Source.Load.Parser where
import Data.Functor.Identity
import Datum.Script.Source.Exp
import Datum.Script.Source.Load.Token           (Token(..))
import Text.Parsec                              (SourcePos, (<?>))
import Data.Repa.Scalar.Date32                  (Date32)
import qualified Datum.Script.Source.Load.Token as K
import qualified Datum.Script.Source.Load.Lexer as L
import qualified Datum.Data.Tree.Exp            as T
import qualified Text.Parsec                    as P
import qualified Text.Parsec.Pos                as P
import qualified Data.Text                      as Text

-------------------------------------------------------------------------------
type Parser a 
        = P.ParsecT [L.Located K.Token] () Identity a

-- | Run parser on some input tokens.
runParser 
        :: FilePath 
        -> [L.Located K.Token]
        -> Parser a 
        -> Either P.ParseError a

runParser filePath tokens p 
 = P.parse p filePath 
 $ filter (not . isKComment . L.locatedBody) tokens
 where
        isKComment (KComment _) = True
        isKComment _            = False


-- | Yield a source position corresponding to the start of the given file.
startingSourcePos :: FilePath -> SourcePos
startingSourcePos filePath
        = P.newPos filePath 1 1


-------------------------------------------------------------------------------
-- | Parse an entire datum script.
pModule  :: Parser Module
pModule 
 = do   _               <- pTok KBraceBra
        ts              <- fmap (map snd) $ P.sepEndBy pTop (pTok KSemi)
        _               <- pTok KBraceKet
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
        let vtsArgs     = [(v, Nothing) | v <- vsArgs]
        return  (sp, TBind vName vtsArgs xBody)


-------------------------------------------------------------------------------
-- | Parse an expression.
pExp :: Parser (SourcePos, Exp)
pExp 
 = pExpApp



-------------------------------------------------------------------------------
-- | Parse a function application.
pExpApp :: Parser (SourcePos, Exp)
pExpApp
 = do   spx     <- P.many1 pExpAtomProj
        let (sps, xs) = unzip spx
        let (sp1 : _) = sps
        case xs of
         [x]    -> return (sp1, x)
         _      -> return (sp1, XDefix xs)

 <?> "an expression or application"


-------------------------------------------------------------------------------
pExpAtomProj :: Parser (SourcePos, Exp)
pExpAtomProj
 = do   (sp, xBody) <- pExpAtom

        P.choice
         [ do   _sp             <- pTok KDot
                (_, nName)      <- pVar
                return  ( sp
                        , makeXApps (XVar (Text.pack "record-project#")) 
                                  [ XFrag (PVData (PDName nName))
                                  , xBody ])

         , do   return  (sp, xBody)
         ]


pExpAtom :: Parser (SourcePos, Exp)
pExpAtom 
 = P.choice
 [      -- parenthesised expression
   do   _       <- pTok KRoundBra
        spx     <- pExp
        _       <- pTok KRoundKet
        return  spx


        -- lambda abstraction
 , do   sp      <- pTok KLam
        (_,  n) <- pVar
        _       <- pTok KRightArrow
        (_,  x) <- pExp
        return  (sp, XAnnot sp $ XAbs n Nothing x)


 , do   -- let-expression
        sp              <- pTok (KKey "if")
        (_, xScrut)     <- pExp
        _               <- pTok (KKey "then")
        (_, xThen)      <- pExp
        _               <- pTok (KKey "else")
        (_, xElse)      <- pExp
        return  (sp, XAnnot sp $ XIf  xScrut xThen xElse)


        -- do expression
 , do   sp      <- pTok (KKey "do")
        _       <- pTok KBraceBra
        ss      <- P.sepEndBy1 pStmt (pTok KSemi)
        _       <- pTok KBraceKet

        case reverse ss of
         (SStmt xE : ssFront)
           -> return (sp, XAnnot sp $ XDo (reverse ssFront) xE)

         _ -> fail "malformed do expression"

        -- record        
 , do   sp      <- pTok KBraceBra

        fs      <- flip P.sepBy1 (pTok KComma)
                $  do   n       <- fmap snd $ pVar

                        -- type annotation on field
                        mt      <- P.choice
                                [ do    _       <- pTok KColon
                                        (_, t)  <- pExpAtom
                                        return $ Just t

                                , do    return Nothing ]

                        _       <- pTok (KOp "=")
                        x       <- fmap snd $ pExp 
                        return (n, mt, x)

        let xRecord
                = foldr (\(n1, mt, x1) xRest 
                           -> let xn = XFrag (PVData (PDName n1))
                                  xt = case mt of 
                                        Just t  -> t
                                        Nothing -> XFrag (PTType (PTAtom T.ATText))
                              in  makeXApps 
                                    (XFrag (PVOp PPRecordExtend))
                                    [ xt, xn, x1, xRest])
                        (XFrag (PVOp PPRecordEmpty))
                        fs

        _       <- pTok KBraceKet
        return  (sp, xRecord)


        -- array
 , do   sp      <- pTok KSquareBra
        xs      <- fmap (map snd) $ P.sepBy1 pExp (pTok KComma)

        let xList 
                = foldr (\x1 xRest 
                           -> XApp (XApp (XFrag (PVOp PPArrayExtend)) x1) xRest)
                        (XFrag (PVOp PPArrayEmpty))
                        xs

        _        <- pTok KSquareKet
        return  (sp, xList)


        -- branch path sugar
 , do   sp      <- pTok KSlashForward
        ns      <- fmap (map snd) $ P.sepBy1 pVar (pTok KSlashForward)
        let xs   = [PDName n | n <- Text.pack "root" : ns]
        let hole = XPrim (PHole (XPrim PKData))
        return  (sp, XAnnot sp $ XFrag (PVData (PDArray hole xs)))


        -- variables
 , do   (sp, u) <- pVar
        return  (sp, XAnnot sp $ XVar u) 


        -- infix operators.
 , do   (sp, u) <- pOp
        return  (sp, XAnnot sp $ XInfixOp u) 


        -- symbols
 , do   (sp, s) <- pSymbol
        return  (sp, XAnnot sp $ XFrag (PVData (PDName (Text.pack s))))

        -- literal unit
 , do   sp      <- pTok KUnit
        return  (sp, XAnnot sp $ XPrim PVUnit)

        -- literal integer
 , do   (sp, n)   <- pLitInt
        return  (sp, XAnnot sp $ XFrag (PVData (PDAtom (T.AInt n))))

        -- literal text
 , do   (sp, str) <- pLitString
        return  (sp, XAnnot sp $ XFrag (PVData (PDAtom (T.AText str))))

        -- literal date
 , do   (sp, d)   <- pLitDate
        return  (sp, XAnnot sp $ XFrag (PVData (PDAtom (T.ADate d))))


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


-- | Parse a literal date.
pLitDate :: Parser (SourcePos, Date32)
pLitDate = pTokMaybe 
        $ \k -> case k of
                 KLitDate d     -> Just d
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
        (K.sayToken . L.locatedBody)
        L.locatedSourcePos
        (\l -> case f (L.locatedBody l) of
                Nothing -> Nothing
                Just a  -> Just (L.locatedSourcePos l, a))

