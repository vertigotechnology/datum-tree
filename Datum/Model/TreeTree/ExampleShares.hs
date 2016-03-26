
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE UndecidableInstances              #-}

module Datum.Model.Tree
        ( module Text.Show.Pretty
        , module Data.Repa.Scalar.Product
        , module Data.Repa.Scalar.Singleton.Nat

        , pp
        , T (..))
where
import Text.Show.Pretty
import Data.Repa.Scalar.Product
import Data.Repa.Scalar.Singleton.Nat
import qualified Data.List                      as L

pp x = putStrLn $ ppShow x

data T k v
        = T k [v]
        deriving Show



data Root = Root
deriving instance Show Root

type Symbol = String
type Time   = String
type Price  = Double
type Volume = Int


trans2
 = T Root
        [ T ("BHP" :*: ())
                [ T (32.16 :*: 1000 :*: "10:01:00" :*: ()) []
                , T (55.16 :*:  415 :*: "10:01:00" :*: ()) [] ]
        ]