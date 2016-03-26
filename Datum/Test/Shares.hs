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
                (TT [ ("symbol",        TPText)
                    , ("name",          TPText) ])

                [ BT "transaction" 
                        (TT [ ("price",         TPDecimal)
                            , ("volume",        TPNat)
                            , ("time",          TPTime) ])
                        []

                , BT "office"
                        (TT  [ ("address",      TPText)])
                        []
                ]
        ]


bShares :: Branch
bShares
 = B    (T [])
        [ [ B  (T [LText "BHP", LText "BHP Billiton Ltd."])
                  [ [ B  (T [LDecimal 32.16, LNat  1000, LTime "10:01:00"]) []
                    , B  (T [LDecimal 55.16, LNat   415, LTime "10:01:00"]) []
                    , B  (T [LDecimal 32.16, LNat 35344, LTime "10:01:00"]) [] 
                    ]

                  , [ B  (T [LText "bhp address1"]) [] 
                    ]
                  ]

          , B  (T [LText "TLS", LText "Telstra Corporation Ltd."])
                  [ [ B  (T [LDecimal 5.11,  LNat   13,  LTime "10:01:05"]) []
                    , B  (T [LDecimal 5.12,  LNat  100,  LTime "10:01:05"]) []
                    ]

                  , [ B  (T [LText "telstra address1"]) []
                    , B  (T [LText "telstra address2"]) []
                    ]
                  ]
          ]
        ]


data Tree
        = Tree Branch BranchType

data Key
        = Key  Tuple  TupleType

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