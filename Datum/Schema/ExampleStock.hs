{-# LANGUAGE ParallelListComp #-}

module Datum.Schema.ExampleShares
where
import Datum.Schema.PrettyCtor
import Datum.Schema.Operator
import Datum.Schema.Check
import Datum.Schema.Exp


---------------------------------------------------------------------------------------------------
tStock :: Tree
tStock = Tree bStock btStock


btStock :: BranchType
btStock  
 = BT   "_root" 
        (TT [])
        [ BT "company" 
                (TT [ ("symbol", ATText)
                    , ("name",   ATText) ])

                [ BT "transaction" 
                        (TT [ ("time",          ATTime)
                            , ("price",         ATDecimal)
                            , ("volume",        ATNat) ])
                        []

                , BT "office"
                        (TT  [ ("address",      ATText)])

                        [ BT "employee"
                                (TT [ ("name",          ATText) ])
                                [ BT "contact"
                                        (TT [ ("sort",   ATText)
                                            , ("number", ATText) ])
                                        []
                                ]
                        ]
                ]
        ]


bStock :: Branch
bStock
 = B    (T [])
        [ [ B   (T [AText "BHP", AText "BHP Billiton Ltd."])
                [ [ B   (T [ATime "10:01:00", ADecimal 32.16, ANat  1000]) []
                  , B   (T [ATime "10:01:00", ADecimal 55.16, ANat   415]) []
                  , B   (T [ATime "10:01:00", ADecimal 32.16, ANat 35344]) [] 
                  ]

                , [ B   (T [AText "171 Collins Street, Melbourne"]) 

                        [ [ B   (T [AText "Max"])   
                                [ [ B   (T [ AText "work", AText "0411123123"]) []
                                  , B   (T [ AText "home", AText "0412321321"]) []
                                  ]
                                ]

                          , B   (T [AText "Eve"])
                                [ [ B   (T [ AText "work", AText "0400999999"]) []
                                  ]
                                ]
                          ]
                        ]
                  ]
                ]

          , B  (T [AText "TLS", AText "Telstra Corporation Ltd."])
                  [ [ B  (T [ATime "10:01:05", ADecimal 5.11, ANat   13]) []
                    , B  (T [ATime "10:01:05", ADecimal 5.12, ANat  100]) []
                    ]

                  , [ B  (T [AText "242 Exhibition Street, Melbourne"]) 
                         [ []
                         ]

                    , B  (T [AText "99 King Street, Sydney"]) 
                         [ []
                         ]
                    ]
                  ]
          ]
        ]


---------------------------------------------------------------------------------------------------
-- | Pretty print the tree.
ex0     = ppTree tStock


-- | Check that a tree is well formed.
ex1     = checkTree tStock


-- | Flatten the tree into a list of path keys.
--
--   Path keys are formed by concatenating the keys from the root 
--   to each leaf of the tree.
--
ex2     = ppKeyList $ keysOfTree tStock


