
module Datum.Script.Eval.Value
        ( Thunk (..)
        , PAP   (..)
        , trimThunk
        , freeVarsX)
where
import Datum.Script.Eval.Env
import Datum.Script.Core.Exp
import Data.Maybe
import Data.Set                         (Set)
import qualified Data.Set               as Set
import Prelude hiding (lookup)


---------------------------------------------------------------------------------------------------


trimThunk :: Thunk -> Thunk
trimThunk (VClosure x env)
 = let  fvs     = freeVarsX Set.empty x

        keep uu
         = case uu of
                UName n -> Set.member n fvs
                _       -> True

        env'    = trim keep env

   in   VClosure x env'

trimThunk (VPAP (PAP p ts))
 = VPAP (PAP p (map trimThunk ts))


-- | Determine the set of unbound variables that are not
--   present in the given environment.
--
--   TODO: handle debruijn vars, or deny them in evaluator.
--
freeVarsX :: Set Name -> Exp -> Set Name
freeVarsX env xx 
 = case xx of
        XAnnot _ x      -> freeVarsX env x
        XPrim{}         -> Set.empty

        XVar (UIx _)    -> Set.empty
        XVar (UName n)
         | Set.member n env     
                        -> Set.empty
         | otherwise    -> Set.singleton n

        XCast _ x       -> freeVarsX env x

        XAbs (BName n) _ x      
         -> freeVarsX (Set.insert n env) x

        XAbs BAnon _ x
         -> freeVarsX env x

        XApp x1 x2
         -> Set.union (freeVarsX env x1) (freeVarsX env x2)

        XRec bxs x2
         -> let (bs, xs) = unzip bxs
                ns       = mapMaybe takeNameOfBind bs
                env'     = Set.union env (Set.fromList ns) 
            in  Set.unions 
                 $ map (freeVarsX env')
                 $ x2 : xs



takeNameOfBind :: Bind -> Maybe Name
takeNameOfBind bb
 = case bb of
        BAnon{} -> Nothing
        BName n -> Just n

