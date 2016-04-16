{-# LANGUAGE ParallelListComp #-}

module Stock
where
import Datum.Data.Tree
import Datum.Data.Tree.SExp
import Datum.Console            (dump)
import qualified Datum.Console  as C

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
ex1     = C.check tStock


-- | Flatten the tree into a list of path keys.
--
--   Path keys are formed by concatenating the keys from the root 
--   to each leaf of the tree.
--
ex2     = dump 
        $ map takeData
        $ keysOfTree tStock


-- | Filter top-level of tree to keep only named dimensions.
ex3_1   = dump 
        $ filterForests (hasName "company")  tStock

ex3_2   = dump 
        $ filterForests (hasName "exchange") tStock


-- | Filter second level of tree to keep only named dimensions.
ex4_1   = dump
        $ mapTrees (filterForests (hasName "transaction")) tStock

ex4_2   = dump
        $ mapTrees (filterForests (hasName "office"))      tStock


-- | Slice tree to retain only data on the given paths.
ex5_1   = dump
        $ sliceTree (\p _ -> onPath ["company", "office", "contact"] p) 
                mempty tStock

ex5_2   = dump
        $ sliceTree (\p _ -> onPath ["company", "office", "employee", "position"] p) 
                mempty tStock

ex5_3   = dump
        $ sliceTree (\p _ -> onPath ["company", "office", "employee", "contact"] p) 
                mempty tStock

ex5_4   = dump
        $ sliceTree (\p _ -> onPath ["company", "transaction"] p)
                mempty tStock


-- | Gather data on given path,
--   unlike 'sliceTree' the matching sub-trees are pulled up 
--   to the top-level.
ex6_1   = dump
        $ gatherTree ["root", "company", "transaction"] tStock

ex6_2   = dump
        $ gatherTree ["root", "company", "office"] tStock

ex6_3   = dump
        $ gatherTree ["root", "company", "office", "employee", "contact"] tStock


