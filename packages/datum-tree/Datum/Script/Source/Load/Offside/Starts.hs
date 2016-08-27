
module Datum.Script.Source.Load.Offside.Starts
        (addStarts)
where
import Datum.Script.Source.Load.Token
import Datum.Script.Source.Load.Located
import Datum.Script.Source.Load.Offside.Base


-- | Add block and line start tokens to this stream.
addStarts :: [Located Token] -> [Lexeme]
addStarts ts
 = case dropInitialLocated ts of
        -- If the first lexeme of a module is not '{' then start a new block.
        (t1 : tsRest)
          |  not $ isToken t1 KBraceBra
          -> LexemeStartBlock (columnOfLocated t1)
                : addStarts' (t1 : tsRest)

          |  otherwise
          -> addStarts' (t1 : tsRest)

        -- empty file
        []      -> []


addStarts'  ::[Located Token] -> [Lexeme]
addStarts' ts

        -- Block started at end of input.
        | Just (ts1, ts2)       <- splitBlockStart ts
        , []                    <- dropInitialLocated ts2
        = [LexemeToken t | t <- ts1] 
                ++ [LexemeStartBlock 0]

        -- Standard block start.
        --  If there is not an open brace after a block start
        --  sequence then insert a new one.
        | Just (ts1, ts2)       <- splitBlockStart ts
        , t2 : tsRest           <- dropInitialLocated ts2
        , not $ isToken t2 KBraceBra
        = [LexemeToken t | t <- ts1]
                ++ [LexemeStartBlock (columnOfLocated t2)]
                ++ addStarts' (t2 : tsRest)

        -- check for start of list
        | t1 : ts'              <- ts
        , isToken t1 KBraceBra
        = LexemeToken t1   
                : addStarts' ts'

        -- check for end of list
        | t1 : ts'              <- ts
        , isToken t1 KBraceKet
        = LexemeToken t1
                : addStarts' ts'

        -- check for start of new line
        | t1 : ts'              <- ts
        , isToken t1 KNewLine
        , t2 : tsRest   <- dropInitialLocated ts'
        , not $ isToken t2 KBraceBra
        = LexemeStartLine (columnOfLocated t2) 
                : addStarts' (t2 : tsRest)

        -- eat up trailine newlines
        | t1 : ts'              <- ts
        , isToken t1 KNewLine
        = addStarts' ts'

        -- a regular token
        | t1 : ts'              <- ts
        = LexemeToken t1
                : addStarts' ts'

        -- end of input
        | otherwise
        = []


-- | Check if a token is one that starts a block of statements.
splitBlockStart 
        :: [Located Token] 
        -> Maybe ([Located Token], [Located Token])

splitBlockStart toks

 | t1@(Located _ _ (KKey "do")) : ts <- toks
 = Just ([t1], ts)
 
 | otherwise                                             
 = Nothing

