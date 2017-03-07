--------------------------------------------------------------------------------
-- An specific monad with error and state
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module Mon
  ( Mon
  , ErrorMsg
  , runMon
  , getCurrentSubst
  , setCurrentSubst
  , newTypeVar
  , getAssumptions
  , withModifiedAssumptions
  ) where

import MonadRSE
import Type
import TypeSubst(TypeSubst)
import TypeVarName(TypeVarName)
import qualified TypeVarName
import qualified TypeSubst
import Assumptions

-- The state holds the current substitution and the type
-- variable to be next generated
data MonState = MkST TypeSubst TypeVarName

type Mon a = MRSE Assumptions MonState a

runMon :: Assumptions -> Mon a -> a
runMon assump m = snd . runMRSE m assump $ initialST
  where initialST = MkST TypeSubst.empty (TypeVarName.fromInt 0)

newTypeVar :: Mon Type
newTypeVar = do
  MkST subst vn <- readST
  writeST (MkST subst (TypeVarName.next vn))
  return (TypeVar vn)

getCurrentSubst :: Mon TypeSubst
getCurrentSubst = do
  MkST subst vn <- readST
  return subst

setCurrentSubst :: TypeSubst -> Mon ()
setCurrentSubst subst = do
  MkST _ vn <- readST
  writeST (MkST subst vn)

getAssumptions :: Mon Assumptions
getAssumptions = reader

withModifiedAssumptions :: Mon a -> (Assumptions -> Assumptions) -> Mon a
withModifiedAssumptions m f = do
  assump <- getAssumptions
  currSubst <- getCurrentSubst
  m `withReader` TypeSubst.apply currSubst (f assump)
