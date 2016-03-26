{-# LANGUAGE ParallelListComp #-}

module Datum.Test.Shares
where
import Datum.Schema.Check
import Datum.Schema.Exp
import Datum.Schema.Type



btShares :: BranchType
btShares  
 = BT "" 
        ( TT [])
        [ BT "company" 
                (TT [ ("symbol",        ATText)
                    , ("name",          ATText) ])

                [ BT "transaction" 
                        (TT [ ("price",         ATDecimal)
                            , ("volume",        ATNat)
                            , ("time",          ATTime) ])
                        []

                , BT "office"
                        (TT  [ ("address",      ATText)])
                        []
                ]
        ]


bShares :: Branch
bShares
 = B    (T [])
        [ [ B  (T [AText "BHP", AText "BHP Billiton Ltd."])
                  [ [ B  (T [ADecimal 32.16, ANat  1000, ATime "10:01:00"]) []
                    , B  (T [ADecimal 55.16, ANat   415, ATime "10:01:00"]) []
                    , B  (T [ADecimal 32.16, ANat 35344, ATime "10:01:00"]) [] 
                    ]

                  , [ B  (T [AText "bhp address1"]) [] 
                    ]
                  ]

          , B  (T [AText "TLS", AText "Telstra Corporation Ltd."])
                  [ [ B  (T [ADecimal 5.11, ANat   13,   ATime "10:01:05"]) []
                    , B  (T [ADecimal 5.12, ANat  100,   ATime "10:01:05"]) []
                    ]

                  , [ B  (T [AText "telstra address1"]) []
                    , B  (T [AText "telstra address2"]) []
                    ]
                  ]
          ]
        ]


tShares = Tree bShares btShares

data Tree
        = Tree Branch BranchType
        deriving Show

data Key
        = Key  Tuple  TupleType
        deriving Show


mapKey :: (Key -> Key) -> Tree -> Tree
mapKey f (Tree (B t sub) (BT name tt tSub))
 = case f (Key t tt) of
        Key t' tt'      -> Tree (B t' sub) (BT name tt' tSub)

-- ??
-- mapSub

{-
broadcast 
        :: [Name] 
        -> [Name]
        -> Tree -> Shape 
        -> (Tree,  Shape)

broadcast ns ds 
        (Tree tup@(Tup xs) subs) 
        shape@(TS nDim (TT nts) tSubs)
 = let  
        ls      = [x    | x      <- xs
                        | (n, t) <- nts 
                        , elem n ns ]

        tree'   = Tree  tup
                        [ if elem nDim ds 
                           then [Tree (Tup (ks ++ xs)) subs' 
                                        | Tree (Tup ks) subs'  <- ts]
                           else ts

                                | ts             <- subs
                                | TS nDim _ _   <- tSubs
                        ]

  in    (tree', shape)
-}