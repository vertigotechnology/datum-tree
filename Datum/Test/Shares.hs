{-# LANGUAGE ParallelListComp #-}

module Datum.Test.Shares
where
import Datum.Schema.Check
import Datum.Schema.Exp
import Datum.Schema.Type



sShares :: Shape
sShares  
 = TS "" 
        (TT [])
        [ TS "company" 
                (TT [ ("symbol",        TPText)
                    , ("name",          TPText) ])

                [ TS "transaction" 
                        (TT [ ("price",         TPDecimal)
                            , ("volume",        TPNat)
                            , ("time",          TPTime) ])
                        []

                , TS "office"
                        (TT  [ ("address",      TPText)])
                        []
                ]
        ]


tShares :: Tree
tShares
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


{-
mapc    :: (Tree -> Tree) 
        -> Tree  -> Tree
mapc f (Tree tup cs


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