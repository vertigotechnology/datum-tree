
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



exShares :: Tree
exShares
 = Tree (Tup [])
        [ [ Tree  (Tup [LText "BHP", LText "BHP Billiton Ltd."])
                  [ [ Tree  (Tup [LDecimal 32.16, LNat  1000, LTime "10:01:00"]) []
                    , Tree  (Tup [LDecimal 55.16, LNat   415, LTime "10:01:00"]) []
                    , Tree  (Tup [LDecimal 32.16, LNat 35344, LTime "10:01:00"]) [] 
                    ]

                  , [ Tree  (Tup [LText "bhp address1"]) [] 
                    ]
                  ]

          , Tree  (Tup [LText "TLS", LText "Telstra Corporation Ltd."])
                  [ [ Tree  (Tup [LDecimal 5.11,  LNat   13,  LTime "10:01:05"]) []
                    , Tree  (Tup [LDecimal 5.12,  LNat  100,  LTime "10:01:05"]) []
                    ]

                  , [ Tree  (Tup [LText "telstra address1"]) []
                    , Tree  (Tup [LText "telstra address2"]) []
                    ]
                  ]
          ]
        ]





