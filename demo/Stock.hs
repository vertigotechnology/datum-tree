{-# LANGUAGE ParallelListComp #-}

module Stock
where
import Datum.Data.Tree
import Datum.Data.Tree.SExp
import Datum.Console

---------------------------------------------------------------------------------------------------
tStock :: Tree O
tStock =  let Right t' = tStock' in t'

tStock'
 = tree 
  (tbranch "root" 
        (ttuple  (telement "name" ttext))
        (tbranch "company"
                (ttuple (telement "symbol" ttext)
                        (telement "name"   ttext))
                (tbranch "transaction" 
                        (ttuple (telement "time"   ttime)
                                (telement "price"  tdecimal)
                                (telement "volume" tnat)))
                (tbranch "office"
                        (ttuple (telement "address" ttext))
                        (tbranch "employee"
                                (ttuple (telement "name" ttext))
                                (tbranch "contact"
                                        (ttuple (telement "sort" ttext)
                                                (telement "number" ttext)))
                                (tbranch "position"
                                        (ttuple (telement "name" ttext))))
                        (tbranch 
                                "contact"
                                (ttuple (telement "sort" ttext)
                                        (telement "number" ttext)))))
        (tbranch "exchange"
                (ttuple (telement "abbrev" ttext)
                        (telement "name"   ttext))))
  (branch 
        (tuple  (text "Stock Market Example"))
        (group  "company"
        (branch (tuple  (text "BHP") (text "BHP Billiton Ltd."))
                (group  "transaction"
                (tuple  (time "10:01:00") (decimal 32.16) (nat 1000))
                (tuple  (time "10:01:00") (decimal 55.16) (nat   415))
                (tuple  (time "10:01:00") (decimal 32.16) (nat 35344)))
                (group  "office"
                (branch (tuple  (text "171 Collins Street, Melbourne"))
                        (group  "employee"
                        (branch (tuple  (text "Max"))
                                (group  "contact"
                                (tuple  (text "work") (text "0411123123"))
                                (tuple  (text "home") (text "0412321321")))
                                (group  "position"
                                (tuple  (text "Disk Jockey"))
                                (tuple  (text "Dragon Slayer"))))
                        (branch (tuple  (text "Eve"))
                                (group  "contact"
                                (tuple  (text "work") (text "0400999999")))
                                (group  "position"
                                (tuple  (text "Data Prophet"))
                                (tuple  (text "Welder")))))
                        (group  "contact"
                        (tuple  (text "security") (text "928312342"))))))
        (branch (tuple  (text "TLS") (text "Telstra Corporation Ltd."))
                (group  "transaction"
                (tuple  (time "10:01:05") (decimal 5.11) (nat 13))
                (tuple  (time "10:01:05") (decimal 5.12) (nat 100)))
                (group  "office"
                (branch (tuple  (text "242 Exhibition Street, Melbourne"))
                        (group  "employee"
                        (branch (tuple  (text "Mario"))
                                (tuple  (text "work") (text "014005551234"))
                                (tuple  (text "Key Master"))))
                        (group  "contact"))
                (branch (tuple (text "99 King Street, Sydney"))
                        (group  "employee"
                        (branch (tuple  (text "Raphael"))
                                (tuple  (text "home") (text "014005550000"))
                                (tuple  (text "Gate Keeper"))))
                        (group   "contact")))))
        (group  "exchange"
        (tuple  (text "ASX")  
                (text "Australian Securities Exchange"))
        (tuple  (text "NYSE") 
                (text "New York Stock Exchange"))))
     

---------------------------------------------------------------------------------------------------
-- | Pretty print the tree.
ex0     = dump tStock


-- | Check that a tree is well formed.
ex1     = checkTree tStock


-- | Flatten the tree into a list of path keys.
--
--   Path keys are formed by concatenating the keys from the root 
--   to each leaf of the tree.
--
ex2     = dump 
        $ keysOfTree tStock


-- | Filter top-level of tree to keep only named dimensions.
ex3_1   = dump 
        $ filterForestsOfTree (\p f -> nameOfForest f == "company")  mempty tStock

ex3_2   = dump 
        $ filterForestsOfTree (\p f -> nameOfForest f == "exchange") mempty tStock


-- | Filter second level of tree to keep only named dimensions.
ex4_1   = dump
        $ mapTreesOfTree 
                (filterForestsOfTree (\p f -> nameOfForest f == "transaction")) 
                mempty tStock

ex4_2   = dump
        $ mapTreesOfTree 
                (filterForestsOfTree (\p f -> nameOfForest f == "office")) 
                mempty tStock


-- | Select data on a given path.
ex5_1   = dump
        $ sliceTree (\p _ -> onPath ["company", "office", "contact"] p) 
                mempty tStock

ex5_2   = dump
        $ sliceTree (\p _ -> onPath ["company", "office", "employee", "position"] p) 
                mempty tStock

ex5_3   = dump
        $ sliceTree (\p _ -> onPath ["company", "transaction"] p)
                mempty tStock

