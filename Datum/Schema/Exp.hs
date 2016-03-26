
module Datum.Schema.Exp
        ( Branch        (..)
        , Tuple         (..)
        , Atom          (..))
where


-- | Trees with a key and sub trees.
data Branch
        = B Tuple [[Branch]]
        deriving Show 


-- | Tuple values.
data Tuple
        = T [Atom]
        deriving Show


-- | Atomic values.
data Atom
        = AUnit
        | ABool         Bool
        | AInt          Int
        | AFloat        Double
        | ANat          Int
        | ADecimal      Double
        | AText         String
        | ATime         String
        deriving Show

