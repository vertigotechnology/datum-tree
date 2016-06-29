
-- | Convert infix expressions to prefix form.
--
--   The parser packs up sequences of expressions and operators into an 
--   XDefix node, but does not convert them to standard prefix applications.
--   That is the job of this module.
--
--   The parsed code will contain XDefix, XInfixOp and XInfixVar nodes, 
--   which are pretty-printed like this:
--
-- @ [DEFIX| Cons start [DEFIX| enumFromTo [DEFIX| start (INFIXOP "+") 1 ] end ]
-- @
--
--   After applying the transform in this module, all function applications
--   will be in prefix form:
--
-- @ Cons start (enumFromTo ((+) start 1) end)@
--
module Datum.Script.Source.Transform.Defix
        ( FixTable      (..)
        , FixDef        (..)
        , InfixAssoc    (..)
        , defaultFixTable
        , Error         (..)
        , Defix         (..))
where
import Datum.Script.Source.Transform.Defix.FixTable
import Datum.Script.Source.Transform.Defix.Error
import Datum.Script.Source.Exp
import Datum.Data.List
import Control.Monad
import Data.Maybe


-- Defix ----------------------------------------------------------------------
class Defix (c :: * -> *) l where
 -- | Resolve infix expressions in a thing.
 defix  :: FixTable l
        -> GXAnnot l
        -> c l
        -> Either (Error l) (c l)


instance GXBound l ~ Name => Defix GModule l where
 defix table a mm
  = case mm of
        Module ts       -> fmap Module $ mapM (defix table a) ts


instance GXBound l ~ Name => Defix GTop l where
 defix table a tt
  = case tt of
        TBind v vs x    -> liftM (TBind v vs) (defix table a x)


instance GXBound l ~ Name => Defix GExp l where
 defix table a xx
  = let down = defix table a
    in case xx of
        XAnnot a' x     -> liftM  (XAnnot a') (defix table a' x)
        XPrim{}         -> return xx
        XVar{}          -> return xx
        XCast c x       -> liftM  (XCast c)   (down x)
        XAbs b t x      -> liftM  (XAbs  b t) (down x)
        XApp x1 x2      -> liftM2  XApp       (down x1) (down x2)
        XLet b mt x1 x2 -> liftM2 (XLet b mt) (down x1) (down x2)

        XDefix xs     
         -> do  xs'     <- mapM down xs
                xs_apps <- defixApps table a xs'
                defixExps table a xs_apps

        XInfixOp{}      -> return xx
        
        XInfixVar str
         -> case lookupDefInfixOfSymbol table str of
                Just def -> return (fixDefExp def a)
                Nothing  -> Left $ ErrorNoInfixDef a str


-------------------------------------------------------------------------------
-- | Preprocess the body of an XDefix node to insert applications.
--    
--   Takes         f a  +  g b  with five  nodes in the XDefix list.
--   and produces (f a) + (g b) with three nodes in the XDefix list.
--
defixApps 
        :: (GXBound l ~ Name)
        => FixTable l   -- ^ Table defining fixities of infix operators.
        -> GXAnnot  l   -- ^ Annotation for constructed expressions.
        -> [GExp l]     -- ^ Sequence of expressions to defix.
        -> Either (Error l) [GExp l]

defixApps table a xx
 = start xx
 where
        -- No expressions, we're done.
        start [] 
         = return []

        -- Single element, we're done.
        start [x]
         = return [x]

        -- Starting operator must be infix .
        start (x1 : xs)
         | Just (a', op) <- takeXAnnotInfixOp a x1
         = case lookupDefPrefixOfSymbol table op of
                Just def -> munch (fixDefExp def a') xs
                Nothing  -> Left $ ErrorMalformed a' (XDefix xx)

        -- Trailing infix operator is malformed.
         | Just (a', _) <- takeXAnnotInfixOp a x1
         , []           <- xs
         = Left $ ErrorMalformed a' (XDefix xx)

        -- Start accumulating an application node.
         | otherwise 
         = munch x1 xs


        -- Munching is done.
        munch acc []
         = return [acc]

        -- We've hit an infix op, drop the accumulated expression.
        munch acc (xop : xs)
         | Just _       <- takeXAnnotInfixOp a xop
         = do   xs'     <- start xs
                return $ acc : xop : xs'

        -- Add another argument to the application.
        munch acc (x1 : xs)
         = munch (XApp acc x1) xs


-------------------------------------------------------------------------------
-- | Defix the body of a XDefix node.
--
--   The input needs to have already been preprocessed by defixApps above.
--
defixExps 
        :: (GXBound l ~ Name)
        => FixTable l   -- ^ Table of infix operator definitions.
        -> GXAnnot  l   -- ^ Annotation of enclosing XDefix.
        -> [GExp l]     -- ^ List of arguments and operators to defix.
        -> Either (Error l) (GExp l)

defixExps table a xx
 = case xx of
        -- If there are no elements then we're screwed.
        -- Maybe the parser is wrong or defixInfix has lost them.
        []      -> Left $ ErrorNoExpressions a
        
        -- If there is only one element then we're done.
        [x]     -> Right x

        -- Keep calling defixInfix until we've resolved all the ops.
        x : xs 
         -> case defixInfix table a xx of
                -- Defixer found errors.
                Left  errs      -> Left errs
                
                -- Defixer didn't find any infix ops, so whatever is leftover
                -- is a standard prefix application.
                Right Nothing   -> Right $ XAnnot a $ makeXApps x xs

                -- Defixer made progress, so keep calling it.
                Right (Just xs') -> defixExps table a xs'


-- | Try to defix a sequence of expressions and XInfixOp nodes.
defixInfix
        :: (GXBound l ~ Name)
        => FixTable l   -- ^ Table of infix operator definitions.
        -> GXAnnot  l   -- ^ Annotation of enclosing XDefix.
        -> [GExp l]     -- ^ List of arguments and operators to defix.
        -> Either (Error l) (Maybe [GExp l])

defixInfix table a xs
        -- Get the list of infix ops in the expression.
        | spOpStrs     <- mapMaybe (takeXAnnotInfixOp a) xs
        = case spOpStrs of
            []     -> Right Nothing
            _      -> defixInfix_ops table a xs 
                   $  map snd spOpStrs

defixInfix_ops 
        :: (GXBound l ~ Name)
        => FixTable l   -- ^ Table of infix operator definitions.
        -> GXAnnot l    -- ^ Annotation of enclosing XDefix.
        -> [GExp l]     -- ^ List of arguments and operators to defix.
        -> [Name]       -- ^ Names of infix operators in the given list.
        -> Either (Error l) (Maybe [GExp l])

defixInfix_ops table sp xs opStrs
   = do   
        -- Lookup infix info for symbols.
        defs            <- mapM (getInfixDefOfSymbol sp table) opStrs
        let precs       = map fixDefPrec  defs
        
        -- Get the highest precedence of all symbols.
        let Just precHigh = takeMaximum precs
   
        -- Get the list of all ops having this highest precedence.
        let opsHigh     = nub
                        $ [ op   | (op, prec) <- zip opStrs precs
                                 , prec == precHigh ]
                                 
        -- Get the list of associativities for just the ops with
        -- highest precedence.
        defsHigh <- mapM (getInfixDefOfSymbol sp table) opsHigh
        let assocsHigh  = map fixDefAssoc defsHigh

        -- All operators at the current precedence level must have the
        -- same associativity, otherwise the implied order-of-operations is
        -- ambiguous.
        case nub assocsHigh of
         [InfixLeft]    
          -> do xs'     <- defixInfixLeft  table sp precHigh xs
                return $ Just xs'

         [InfixRight]   
          -> do xs'     <- defixInfixRight table sp precHigh (reverse xs)
                return $ Just (reverse xs')
         
         [InfixNone]
          -> do xs'     <- defixInfixNone  table sp precHigh xs
                return $ Just (reverse xs')

         _ -> Left $ ErrorDefixMixedAssoc sp opsHigh


-- | Defix some left associative ops.
defixInfixLeft 
        :: (GXBound l ~ Name)
        => FixTable l   -- ^ Table of infix operator definitions
        -> GXAnnot  l   -- ^ Annotation of enclosing XDefix.
        -> Int          -- ^ Precedence of operator to defix.
        -> [GExp l]     -- ^ List of arguments and operators to defix.
        -> Either (Error l) [GExp l]

defixInfixLeft table sp precHigh (x1 : xop : x2 : xs)
        | Just (spo, op) <- takeXAnnotInfixOp sp xop
        , Just def       <- lookupDefInfixOfSymbol table op
        , fixDefPrec def == precHigh
        =       Right (XApp (XApp (fixDefExp def spo) x1) x2 : xs)

        | Just (spo, op) <- takeXAnnotInfixOp sp xop
        = do    xs'      <- defixInfixLeft table sp precHigh (x2 : xs)
                Right   $ x1 : XAnnot spo (XInfixOp op) : xs'

defixInfixLeft _ sp _ xs
        = Left $ ErrorMalformed sp (XDefix xs)


-- | Defix some right associative ops.
--   The input expression list is reversed, so we can eat the operators left
--   to right. We must be careful to build the App node the right way around.
defixInfixRight
        :: (GXBound l ~ Name)
        => FixTable l   -- ^ Table of infix operator definitions.
        -> GXAnnot  l   -- ^ Annotation of enclosing XDefix.
        -> Int          -- ^ Precendence of operator to defix.
        -> [GExp l]     -- ^ List of arguments and operators to defix.
        -> Either (Error l) [GExp l]

defixInfixRight table sp precHigh (x2 : xop : x1 : xs)
        | Just (spo, op) <- takeXAnnotInfixOp sp xop
        , Just def       <- lookupDefInfixOfSymbol table op
        , fixDefPrec def == precHigh
        =       Right (XApp (XApp (fixDefExp def spo) x1) x2 : xs)

        | Just (spo, op) <- takeXAnnotInfixOp sp xop
        = do    xs'     <- defixInfixRight table sp precHigh (x1 : xs)
                Right   $ x2 : XAnnot spo (XInfixOp op) : xs'

defixInfixRight _ sp _ xs
        = Left $ ErrorMalformed sp (XDefix xs)


-- | Defix non-associative ops.
defixInfixNone 
        :: (GXBound l ~ Name)
        => FixTable l   -- ^ Table of infix operator definitions.
        -> GXAnnot  l   -- ^ Annotation of enclosing XDefix.
        -> Int          -- ^ Precedence of operator to defix.
        -> [GExp l]     -- ^ List of arguments and operators to defix.
        -> Either (Error l) [GExp l]

defixInfixNone table sp precHigh xx
        -- If there are two ops in a row that are non-associative and have
        -- the same precedence then we don't know which one should come first.
        | _ : xop2 : _ : xop4 : _ <- xx
        , Just (sp2, op2) <- takeXAnnotInfixOp sp xop2
        , Just (sp4, op4) <- takeXAnnotInfixOp sp xop4
        , Just def2       <- lookupDefInfixOfSymbol table op2
        , Just def4       <- lookupDefInfixOfSymbol table op4
        , fixDefPrec def2 == fixDefPrec def4
        = Left  $ ErrorDefixNonAssoc op2 sp2 op4 sp4

        -- Found a use of the operator of interest.
        | x1 : xop2 : x3 : xs       <- xx
        , Just (sp2, op2) <- takeXAnnotInfixOp sp xop2
        , Just def2       <- lookupDefInfixOfSymbol table op2
        , fixDefPrec def2 == precHigh
        = Right $ (XApp (XApp (fixDefExp def2 sp2) x1) x3) : xs

        -- Some other operator.
        | x1 : xop2 : x3 : xs       <- xx
        , Just (_sp2, _op2) <- takeXAnnotInfixOp sp xop2
        = case defixInfixNone table sp precHigh (x3 : xs) of
                Right xs'       -> Right (x1 : xop2 : xs')
                Left errs       -> Left errs

        | otherwise
        = Left $ ErrorMalformed sp (XDefix xx)


-------------------------------------------------------------------------------
-- | Take the bound variable from possibly annotated XInfixOp.
takeXAnnotInfixOp :: GXAnnot l -> GExp l -> Maybe (GXAnnot l, GXBound l)
takeXAnnotInfixOp a xx
 = case xx of
        XAnnot a' x     -> takeXAnnotInfixOp a' x
        XInfixOp u      -> Just (a, u)
        _               -> Nothing


