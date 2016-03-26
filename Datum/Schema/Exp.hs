
module Datum.Schema.Exp
        ( Branch        (..)
        , Tuple         (..)
        , Literal       (..))
where


-- | Trees with a key and sub trees.
data Branch
        = B Tuple [[Branch]]
        deriving Show 


-- | Tuple values.
data Tuple
        = T [Literal]
        deriving Show


-- | Literal values.
data Literal
        = LUnit
        | LBool         Bool
        | LInt          Int
        | LFloat        Double
        | LNat          Int
        | LDecimal      Double
        | LText         String
        | LTime         String
        deriving Show

