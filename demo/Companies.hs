
module Companies
where
import Datum.Data.Tree.Operator.Extract
import Datum.Data.Tree
import Datum.Console


file    = "demo/data/ASXListedCompanies.csv"


loadCompanies
 = do   t <- loadCSV file
        return  $ mapTrees (renameFields ["name", "symbol", "grouping"]) t
 

-- | Load the file and display a sample.
--   We take 10 rows from the data with uniform spacing.
ex1 
 = do   t <- loadCompanies
        dump    $ sample 10 t


-- | Slurp out a list of tuples, this eliminates the outer tree structure.
ex2
 = do   t <- loadCompanies
        dump    $ map takeData $ keysOfTree $ sample 20 t


-- | Group the companies by industry group.
--   The sampling mechanism will take 5 branches from every group.
ex3
 = do   t <- loadCompanies
        dump    $ sample 5 
                $ mapForests (groupForest "grouping") t


-- | Gathering the leaves lifts them back to the top-level while 
--   preserving the natural order, so the data is still segmented by group.
ex4
 = do   t <- loadCompanies
        dump    $ sample 30
                $ gatherTree ["root", "row", "row"]
                $ mapForests (groupForest "grouping") t


-- | By sampling before grouping we get 5 whole groups of up to 5 elements,
--   rather than just one or two from each group.
ex5
 = do   t <- loadCompanies
        dump    $ map takeData $ keysOfTree
                $ gatherTree ["root", "row", "row"]
                $ sample 5
                $ mapForests (groupForest "grouping") t


-- | Grouping on the symbol instead brings that column to the front.
ex6
 = do   t <- loadCompanies
        dump    $ sample 30
                $ gatherTree ["root", "row", "row"]
                $ mapForests (groupForest "symbol") t

ex7
 = do   t <- loadCompanies

        let (ts :: [Name])
                = mapMaybe (join . fmap extract . elementOfKey "symbol")
                $ filter   (hasElement "grouping" (text "Software & Services"))
                $ keysOfTree t

        dump    $ ts