
module Datum.Script.Eval.Env
        ( Env   (..)
        , Value (..)
        , empty
        , lookup
        , insert)
where
import Datum.Script.Exp.Core
import Data.Map                         (Map)
import qualified Data.Map               as Map
import Prelude hiding (lookup)


-- | Value.
data Value
        -- | An expression with values for free variables
        --   defined by the given environemnt.
        = VClosure !Exp  !Env

        -- | A primitive partially applied to its first few arguments.
        | VPrim    !Prim ![Value]

deriving instance Show Value


-- | Environment holding names of bound variables.
data Env
        = Env
        { envNamed              :: !(Map Name Value)
        , envStack              :: ![Value] 
        , envStackLength        :: !Int }

deriving instance Show Env


-- | The empty environment.
empty  :: Env
empty   = Env Map.empty [] 0


-- | Lookup an expression in the environment.
lookup :: Bound -> Env -> Maybe Value
lookup uu env
 = case uu of
        UIx i
         | i <  0                  -> Nothing
         | i >= envStackLength env -> Nothing
         | otherwise               -> Just (envStack env !! i)

        UName n                    -> Map.lookup n (envNamed env)


-- | Insert a new expression into the environment.
insert :: Bind -> Value -> Env -> Env
insert bb xx env
 = case bb of
        BAnon   -> env { envStack = xx : envStack env }
        BName n -> env { envNamed = Map.insert n xx (envNamed env) }

