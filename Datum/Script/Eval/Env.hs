{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Eval.Env 
        ( Env    (..)
        , Thunk  (..)
        , PAP    (..)
        , empty
        , lookup, insert
        , union
        , trim)
where
import Datum.Script.Core.Exp
import Data.Maybe
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import Prelude hiding (lookup)


-- | Environment holding names of bound variables.
data Env
        = Env
        { envNamed              :: !(Map Name Thunk)
        , envStack              :: ![Thunk] 
        , envStackLength        :: !Int }

deriving instance Show Env


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

