--------------------------------------------------------------------------------
-- An specific monad with error and state
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module Mon
  ( Mon
  , ErrorMsg
  , runMon
  , getCurrentSubst
  , setCurrentSubst
  , newTypeVar
  ) where

import MonadSE
import Type
import TypeSubst(TypeSubst)
import TypeVarName
import qualified TypeSubst

-- The state holds the current substitution and the type
-- variable to be next generated
data MonState = MkST TypeSubst TypeVarName

type Mon a = MSE MonState a

runMon :: Mon a -> a
runMon m = snd . runMSE m  $  initialST
  where initialST = MkST TypeSubst.empty (typeVarName 0)

newTypeVar :: Mon Type
newTypeVar = do
  MkST subst vn <- readST
  writeST (MkST subst (nextTypeVarName vn))
  return (TypeVar vn)

getCurrentSubst :: Mon TypeSubst
getCurrentSubst = do
  MkST subst vn <- readST
  return subst

setCurrentSubst :: TypeSubst -> Mon ()
setCurrentSubst subst =  do
  MkST _ vn <- readST
  writeST (MkST subst vn)
