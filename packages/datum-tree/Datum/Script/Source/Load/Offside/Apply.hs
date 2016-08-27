
module Datum.Script.Source.Load.Offside.Apply
        (applyOffside)
where
import Datum.Script.Source.Load.Offside.Base
import Datum.Script.Source.Load.Located
import Datum.Script.Source.Load.Token


-- | Apply the offside rule to this token stream.
--
--    It should have been processed with addStarts first to add the
--    LexemeStartLine/LexemeStartLine tokens.
--
--    Unlike the definition in the Haskell 98 report, we explicitly track
--    which parenthesis we're inside. We use these to partly implement
--    the layout rule that says we much check for entire parse errors to
--    perform the offside rule.
--
applyOffside 
        :: [Paren]              -- ^ What parenthesis we're inside.
        -> [Context]            -- ^ Current layout context.
        -> [Lexeme]             -- ^ Input lexemes.
        -> [Located Token]

-- Enter into a top-level block in the module, and start applying the 
-- offside rule within it.
applyOffside ps [] (LexemeStartBlock n : ls')
        = newCBra ls' : applyOffside (ParenBrace : ps) [n] ls'

-- At top level without a context.
-- Skip over everything until we get the first block.
applyOffside ps [] (LexemeStartLine _  : ts)
        = applyOffside ps [] ts 

-- line start
applyOffside ps mm@(m : ms) (t@(LexemeStartLine n) : ts)
        -- add semicolon to get to the next statement in this block
        | m == n
        = newSemiColon ts : applyOffside ps mm ts

        -- end a block
        | n <= m 
        = case ps of
                -- Closed a block that we're inside, ok.
                ParenBrace : ps'
                  -> newCKet ts : applyOffside ps' ms (t : ts)

                -- We're supposed to close the block we're inside, but we're 
                -- still inside an open '(' context. Just keep passing the
                -- tokens through, and let the parser give its error when 
                -- it gets to it.
                ParenRound : _
                  -> applyOffside ps ms ts

                -- We always push an element of the layout context
                -- at the same time as a paren context, so this shouldn't happen.
                _ -> error $ "applyOffside: paren / layout context mismatch."

        -- indented continuation of this statement
        | otherwise
        = applyOffside ps mm ts


-- block start
applyOffside ps mm@(m : ms) (LexemeStartBlock n : ts)
        -- enter into a nested context
        | n > m
        = newCBra ts : applyOffside (ParenBrace : ps) (n : m : ms) ts 

        -- new context starts less than the current one.
        --  This should never happen, 
        --    provided addStarts works.
        | tNext : _     <- dropInitialLexeme ts
        = error $ "applyOffside: layout error on " ++ show tNext ++ "."

        -- new context cannot be less indented than outer one
        --  This should never happen,
        --   as there is no lexeme to start a new context at the end of the file.
        | []            <- dropInitialLexeme ts
        = error $ "applyOffside: tried to start new context at end of file."

        -- an empty block
        | otherwise
        = newCBra ts : newCKet ts : applyOffside ps mm (LexemeStartLine n : ts)


-- push context for explicit open brace
applyOffside ps ms 
        (LexemeToken t@(Located _ _ KBraceBra) : ts)
        = t : applyOffside (ParenBrace : ps) (0 : ms) ts

-- pop context from explicit close brace
applyOffside ps mm 
        (LexemeToken t@(Located _ _ KBraceKet) : ts) 

        -- make sure that explict open braces match explicit close braces
        | 0 : ms                <- mm
        , ParenBrace : ps'      <- ps
        = t : applyOffside ps' ms ts

        -- nup
        | _tNext : _     <- dropInitialLexeme ts
        = [newOffsideClose ts]

-- push context for explict open paren.
applyOffside ps ms 
        (    LexemeToken t@(Located _ _ KRoundBra) : ts)
        = t : applyOffside (ParenRound : ps) ms ts

-- force close of block on close paren.
-- This partially handles the crazy (Note 5) rule from the Haskell98 standard.
applyOffside (ParenBrace : ps) (m : ms)
        (lt@(LexemeToken   (Located _ _ KRoundKet)) : ts)
 | m /= 0
 = newCKet ts : applyOffside ps ms (lt : ts)

-- pop context for explicit close paren.
applyOffside (ParenRound : ps) ms 
        (   LexemeToken t@(Located _ _ KRoundKet) : ts)
        = t : applyOffside ps ms ts

-- close off remaining contexts once we've reached the end-of-file marker.
applyOffside ps (_ : ms) ((LexemeToken l@(Located _ _ KEndOfFile)) : ts)
        = (newCKet [] : applyOffside ps ms ts) ++ [l]

-- pass over tokens.
applyOffside ps ms (LexemeToken t : ts) 
        = t : applyOffside ps ms ts

applyOffside _ [] []        = []


-- close off remaining contexts once we've reached the end of the stream.
applyOffside ps (_ : ms) []    
        = newCKet [] : applyOffside ps ms []


