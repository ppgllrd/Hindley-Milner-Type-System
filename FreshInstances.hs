--------------------------------------------------------------------------------
-- Generation of fresh instances of type schemes
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module FreshInstances (newTypeInstance) where

-- ToDo: document this module

import qualified Data.Set as Set
import Expr
import Type
import FreeTypeVars
import Assumptions(Assumptions)
import qualified Assumptions
import Mon
import TypeSubst(TypeSubst)
import qualified TypeSubst
import TypeVarName

newTypeInstance :: VarName -> Assumptions -> Mon Type
newTypeInstance vn assumpt = do
  case Assumptions.lookup vn assumpt of
    Nothing -> fail ("Free variable "++ vn)
    Just (Forall tvns t) -> do
      tvs' <- mapM (const newTypeVar) tvns
      let subst = foldr (TypeSubst.compose . uncurry TypeSubst.singleton)
                        TypeSubst.empty (zip tvns tvs')
      return (TypeSubst.apply subst t)

generalize :: Type -> Assumptions -> PolyType
generalize t assumpt = Forall (Set.toList vars) t
  where
    vars = freeTypeVars t `Set.difference` freeTypeVars assumpt


alphaVarName = typeVarName (-1)

alpha = TypeVar alphaVarName
beta = TypeVar (typeVarName (-2))


ass1 = Assumptions.addPoly "x" (Forall [alphaVarName] alpha) Assumptions.empty


ass2 = Assumptions.add "y" beta ass1

ass3 = Assumptions.add "z" ((alpha *-> boolType) *-> (alpha *-> beta)) ass2

exa1 = runMon (newTypeInstance "z" ass3)

exa2 = runMon (newTypeInstance "x" ass2)

exa3 = runMon (newTypeInstance "y" ass2)
