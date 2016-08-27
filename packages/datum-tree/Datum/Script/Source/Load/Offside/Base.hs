
module Datum.Script.Source.Load.Offside.Base
        ( Lexeme (..)
        , Paren (..)
        , Context
        , stripLexemes
        , dropInitialLocated
        , dropInitialLexeme
        , isToken
        , newCBra
        , newCKet
        , newOffsideClose
        , newSemiColon)
where
import Datum.Script.Source.Load.Token
import Datum.Script.Source.Load.Located
import qualified Text.Lexer.Inchworm.Char       as I


-- | Holds a real token or start symbol which is used to apply the offside rule.
data Lexeme
        -- | Wrap a token from the source file.
        = LexemeToken           (Located Token)

        -- | Signal that we're starting a new line in this column.
        | LexemeStartLine       Int

        -- | Signal that we're starting a block in this column.
        | LexemeStartBlock      Int
        deriving Show


-- | Parenthesis that we're currently inside. 
data Paren
        = ParenRound
        | ParenBrace
        deriving Show


-- | What column number the current layout context started in.
type Context
        = Int


-- | Strip Lexemes back to Located tokens, discarding the extra block
--   start information.
stripLexemes :: [Lexeme] -> [Located Token]
stripLexemes []                   = []
stripLexemes (LexemeToken l : ls) = l : stripLexemes ls
stripLexemes (_ : ls)             = stripLexemes ls


-- | Drop initial tokens at the front of this stream that we don't use.
dropInitialLocated :: [Located Token] -> [Located Token]
dropInitialLocated [] = []
dropInitialLocated (t1 : ts)
 = case locatedBody t1 of
        KNewLine        -> dropInitialLocated ts
        KComment _      -> dropInitialLocated ts
        _               -> t1 : ts



-- | Drop newline tokens at the front of this stream.
dropInitialLexeme :: [Lexeme] -> [Lexeme]
dropInitialLexeme []    = []

dropInitialLexeme (LexemeToken t1 : ls)
 = case locatedBody t1 of
        KNewLine        -> dropInitialLexeme ls
        KComment _      -> dropInitialLexeme ls
        _               -> LexemeToken t1 : ls

dropInitialLexeme (l : ls)
 = l : dropInitialLexeme ls


-- | Test whether this wrapper token matches.
isToken :: Located Token -> Token -> Bool
isToken (Located _ _ tok) tok2
        = tok == tok2


-- | Make a new open brace,
--   using the first available source location in the given list of lexemes.
newCBra      :: [Lexeme] -> Located Token
newCBra ts
 = case takeFirstToken ts of
        Located f sp _
         -> Located f sp KBraceBra


-- | Make a new close brace,
--   using the first available source location in the given list of lexemes.
newCKet      :: [Lexeme] -> Located Token
newCKet ts
 = case takeFirstToken ts of
        Located f sp _  
         -> Located f sp KBraceKet


-- | Make a new close brace,
--   using the first available source location in the given list of lexemes.
newOffsideClose      :: [Lexeme] -> Located Token
newOffsideClose ts
 = case takeFirstToken ts of
        Located f sp _  
         -> Located f sp KOffsideClose


-- | Make a new semicolon,
--   using the first available source location in the given list of lexemes.
newSemiColon :: [Lexeme] -> Located Token
newSemiColon ts
 = case takeFirstToken ts of
        Located f sp _
         -> Located f sp KSemi


-- | Take the first available non-newline or comment token.
takeFirstToken :: [Lexeme] -> Located Token
takeFirstToken []      
 = Located "" (I.Location 0 0) (KErrorJunk ' ') 

takeFirstToken (l : ls)
 = case l of
        LexemeToken (Located _ _ KNewLine)
         -> takeFirstToken ls

        LexemeToken (Located _ _ (KComment _))
         -> takeFirstToken ls

        LexemeToken t           -> t
        LexemeStartLine  _      -> takeFirstToken ls
        LexemeStartBlock _      -> takeFirstToken ls

