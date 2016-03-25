
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies   #-}

module Datum.Model.Operators
        ( module Text.Show.Pretty
        , module Data.Repa.Scalar.Product
        , module Data.Repa.Scalar.Singleton.Nat

        , pp
        , mapv
        , members
        , group
        , flatten)
where
import Text.Show.Pretty
import Data.Repa.Scalar.Product
import Data.Repa.Scalar.Singleton.Nat
import qualified Data.List                      as L

pp x = putStrLn $ ppShow x


-- | Apply a function to the values of a table of key-value pairs.
mapv :: (a -> b) -> [k :*: a] -> [k :*: b]
mapv f branches
        = [ k :*: f rows | k :*: rows <- branches ]


-- | Get the unique members of a column in a table.
members :: ( Select n ts
           , Eq (Select' n ts))
        => Nat n -> [ts] -> [Select' n ts]
members n xs
        = L.nub $ map (select n) xs


-- | Group rows by their first column.
group   :: ( IsProdList ts, Discard n ts, Select n ts
           , Eq (Select' n ts))
        => Nat n -> [ts] -> [Select' n ts :*: [Discard' n ts]]

group n rows
 = let  keys    = members n rows

        slurp k = map    (discard n) 
                $ filter (\row -> select n row == k)
                $ rows

   in   map (\k -> k :*: slurp k) keys


-- | Flatten dimensions so that they are indexed by the union of the 
--   inner and outer keys.
flatten  :: [k1 :*: [k2 :*: ts]] -> [k1 :*: k2 :*: ts]
flatten xs
        = concat
        $ map (\(k1 :*: k1s)
                -> map (\(k2 :*: k2s)
                        -> k1 :*: k2 :*: k2s) k1s) xs


