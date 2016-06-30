{-# LANGUAGE UndecidableInstances #-}
module Datum.Script.Eval.Env where
import Datum.Script.Core.Exp
import Data.Map                         (Map)
import qualified Data.Map               as Map
import Prelude hiding (lookup)


---------------------------------------------------------------------------------------------------
-- | Thunk can be a closure or an application of a primitive. 
data Thunk
        -- | An expression with values for free variables
        --   defined by the given environemnt.
        = VClosure !Exp  !Env

        -- | A primitive partially applied to its first few arguments.
        | VPrim    !Prim ![Thunk]

deriving instance Show Thunk


-- | Thunk represents an expression in normal form.
isNormalThunk :: Thunk -> Bool
isNormalThunk tt
 = case tt of
        VClosure x _env 
         -> isNormalOpenX arityOfPrim x

        VPrim p xs
         -> length xs >= arityOfPrim p


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

