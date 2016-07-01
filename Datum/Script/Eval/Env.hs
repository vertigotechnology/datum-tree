{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Eval.Env where
import Datum.Script.Core.Exp
import Data.Maybe
import Data.Map                         (Map)
import Data.Set                         (Set)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import Prelude hiding (lookup)


---------------------------------------------------------------------------------------------------
-- | Closure packages up an expression with its environment.
data Thunk
        -- | An expression with values for free variables
        --   defined by the given environemnt.
        = VClosure !Exp !Env

        | VPAP     !PAP


deriving instance Show Thunk


-- | Partially applied primitive.
data PAP
        = PAP !Prim ![Thunk]

deriving instance Show PAP


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


---------------------------------------------------------------------------------------------------
-- | Environment holding names of bound variables.
data Env
        = Env
        { envNamed              :: !(Map Name Thunk)
        , envStack              :: ![Thunk] 
        , envStackLength        :: !Int }

deriving instance Show Env


-- | The empty environment.
empty  :: Env
empty   = Env Map.empty [] 0


-- | Lookup an expression in the environment.
lookup :: Bound -> Env -> Maybe Thunk
lookup uu env
 = case uu of
        UIx i
         | i <  0                  -> Nothing
         | i >= envStackLength env -> Nothing
         | otherwise               -> Just (envStack env !! i)

        UName n                    -> Map.lookup n (envNamed env)


-- | Insert a new expression into the environment.
insert :: Bind -> Thunk -> Env -> Env
insert bb xx env
 = case bb of
        BAnon   -> env { envStack = xx : envStack env }
        BName n -> env { envNamed = Map.insert n xx (envNamed env) }


-- | Take the union of two environments.
union :: Env -> Env -> Env
union env1 env2
        = Env
        { envNamed              = Map.union (envNamed env1) (envNamed env2)
        , envStack              = envStack env2       ++ envStack env1
        , envStackLength        = envStackLength env2 +  envStackLength env1 }


trim :: (Bound -> Bool) -> Env -> Env
trim keep env
        = env
        { envNamed      = Map.filterWithKey (\n _ -> keep (UName n))
                        $ envNamed env 
        }



