{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Eval.Env 
        ( Env    (..)
        , Value  (..)
        , Clo    (..)
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
        { envNamed      :: !(Map Name Value)
        , envStack      :: ![Value] 
        , envStackLen   :: !Int }

deriving instance Show Env


-- | Closure packages up an expression with its environment.
data Value
        -- | Closure 
        = VClo !Clo

        -- | Partially applied primitive.
        | VPAP !PAP

deriving instance Show Value


-- | Closure consisting of an expression and its environment.
data Clo
        = Clo !Exp  !Env

deriving instance Show Clo


-- | Partially applied primitive.
data PAP
        = PAP !Prim ![Value]

deriving instance Show PAP


-- | The empty environment.
empty  :: Env
empty   = Env Map.empty [] 0


-- | Lookup an expression in the environment.
lookup :: Bound -> Env -> Maybe Value
lookup uu env
 = case uu of
        UIx i
         | i <  0               -> Nothing
         | i >= envStackLen env -> Nothing
         | otherwise            -> Just (envStack env !! i)

        UName n                 -> Map.lookup n (envNamed env)
        _                       -> Nothing

-- | Insert a new expression into the environment.
insert :: Bind -> Value -> Env -> Env
insert bb xx env
 = case bb of
        BAnon           -> env { envStack = xx : envStack env }
        BName n         -> env { envNamed = Map.insert n xx (envNamed env) }
        _               -> env


-- | Take the union of two environments.
union :: Env -> Env -> Env
union env1 env2
        = Env
        { envNamed      = Map.union (envNamed env1) (envNamed env2)
        , envStack      = envStack    env2 ++ envStack    env1
        , envStackLen   = envStackLen env2 +  envStackLen env1 }


trim :: (Bound -> Bool) -> Env -> Env
trim keep env
        = env
        { envNamed      = Map.filterWithKey (\n _ -> keep (UName n))
                        $ envNamed env 
        }

