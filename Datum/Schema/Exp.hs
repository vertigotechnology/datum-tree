
module Datum.Schema.Exp
        ( Tree  (..)
        , Tup   (..)
        , Lit   (..))
where


-- | Trees with a key and sub trees.
data Tree
        = Tree Tup      [[Tree]]
        deriving Show 


-- | Tuple values.
data Tup
        = Tup [Lit]
        deriving Show


-- | Literal values.
data Lit
        = LUnit
        | LBool         Bool
        | LInt          Int
        | LFloat        Double
        | LNat          Int
        | LDecimal      Double
        | LText         String
        | LTime         String
        deriving Show

