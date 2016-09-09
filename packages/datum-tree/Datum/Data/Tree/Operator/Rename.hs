
module Datum.Data.Tree.Operator.Rename
        (renameDimOfTree)
where
import Datum.Data.Tree.Exp


-- | Rename dimensions in a tree. 
renameDimOfTree 
        :: [Name]       -- ^ Source dimension names.
        -> [Name]       -- ^ Result dimension names.
        -> Tree 'O      -- ^ Source tree.
        -> Tree 'O      -- ^ Result tree.

renameDimOfTree nsSource nsResult (Tree b bt)
 = let  bt'      = renameDimOfBranchType nsSource nsResult bt
   in   Tree b bt'
 

renameDimOfBranchType
        :: [Name] -> [Name] -> BranchType -> BranchType

renameDimOfBranchType nsSource nsResult bt

 -- Enter into the named branch.
 | BT nBranch tt bts       <- bt
 , nSource1 : nsSourceRest <- nsSource
 , nResult1 : nsResultRest <- nsResult
 , nBranch  == nSource1
 = BT nResult1 tt 
        $ boxes 
        $ map (renameDimOfBranchType nsSourceRest nsResultRest)
        $ renameDimOfBranchTypes nsSourceRest nsResultRest 
        $ unboxes bts

 -- Either we've reached the end of the given paths,
 -- or the source branch name does not match.
 | otherwise
 = bt


renameDimOfBranchTypes
        :: [Name] -> [Name] -> [BranchType] -> [BranchType]

renameDimOfBranchTypes nsSource nsResult bts
 | (nSource1 : _)       <- nsSource
 = let  
        sel bt@(BT nBranch _ _)
         | nBranch == nSource1
         = renameDimOfBranchType nsSource nsResult bt

        sel bt = bt

   in   map sel bts

 | otherwise
 = bts

