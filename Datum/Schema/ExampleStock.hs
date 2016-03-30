{-# LANGUAGE ParallelListComp #-}

module Datum.Schema.ExampleShares
where
import Datum.Schema.Pretty
import Datum.Schema.Operator
import Datum.Schema.Check
import Datum.Schema.Exp


---------------------------------------------------------------------------------------------------
tStock :: Tree
tStock = Tree bStock btStock


btStock :: BranchType
btStock  
 = BT   "root" 
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

        , BT "exchange"
                (TT [ ("abbrev", ATText)
                    , ("name",   ATText) ])
                []
        ]


bStock :: Branch
bStock
 = B (T [])
     [ G [ B (T [AText "BHP", AText "BHP Billiton Ltd."])
             [ G [ B (T [ATime "10:01:00", ADecimal 32.16, ANat  1000]) []
                 , B (T [ATime "10:01:00", ADecimal 55.16, ANat   415]) []
                 , B (T [ATime "10:01:00", ADecimal 32.16, ANat 35344]) [] 
                 ]

             , G [ B (T [AText "171 Collins Street, Melbourne"]) 
                     [ G [ B (T [AText "Max"])   
                             [ G [ B (T [ AText "work", AText "0411123123"]) []
                                 , B (T [ AText "home", AText "0412321321"]) []
                                 ]
                             ]

                         , B (T [AText "Eve"])
                             [ G [ B (T [ AText "work", AText "0400999999"]) []
                                 ]
                             ]
                         ]
                     ]
                 ]
             ]

         , B (T [AText "TLS", AText "Telstra Corporation Ltd."])
             [ G [ B (T [ATime "10:01:05", ADecimal 5.11, ANat   13]) []
                 , B (T [ATime "10:01:05", ADecimal 5.12, ANat  100]) []
                 ]
             , G [ B (T [AText "242 Exhibition Street, Melbourne"]) 
                     [ G []
                     ]
                 , B (T [AText "99 King Street, Sydney"]) 
                     [ G []
                     ]
                 ]
             ]
         ]

     , G [ B (T [AText "ASX",  AText "Australian Securities Exchange"]) []
         , B (T [AText "NYSE", AText "New York Stock Exchange"]) [] ]
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


-- | Filter top-level of tree to keep only named dimensions.
ex3_1   = ppTree $ filterForestsOfTree (\p f -> nameOfForest f == "company")  mempty tStock
ex3_2   = ppTree $ filterForestsOfTree (\p f -> nameOfForest f == "exchange") mempty tStock


-- | Filter second level of tree to keep only named dimensions.
ex4_1   = ppTree 
        $ mapTreesOfTree 
                (filterForestsOfTree (\p f -> nameOfForest f == "transaction")) 
                mempty tStock

ex4_2   = ppTree 
        $ mapTreesOfTree 
                (filterForestsOfTree (\p f -> nameOfForest f == "office")) 
                mempty tStock

