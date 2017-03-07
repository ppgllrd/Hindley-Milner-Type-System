--------------------------------------------------------------------------------
-- The type analyser
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module TypeInference(typeOf) where

import qualified Data.Set as Set
import Expr
import Type
import Assumptions(Assumptions)
import qualified Assumptions
import Mon
import TypeSubst(TypeSubst)
import qualified TypeSubst
import Unification
import FreeTypeVars

-- ToDo: extend monad with a reader for assumptions

analyse :: Expr -> Assumptions -> Mon Type
analyse (ConstB bool)   assump = return boolType
analyse (ConstI int)    assump = return intType
analyse (Var vnm)       assump = newTypeInstance vnm assump
analyse (App e1 e2)     assump = do
  funType <- analyse e1 assump
  argType <- analyse e2 assump
  resType <- newTypeVar
  unify (argType *-> resType) funType
  return resType
analyse (Abs vn e)      assump = do
  argType <- newTypeVar
  let newAssump = Assumptions.add vn argType assump
  resType <- analyse e newAssump
  return (argType *-> resType)
analyse (Let vn e1 e2) assump = do
  localType <- analyse e1 assump
  localType' <- generalize localType assump
  subst <- getCurrentSubst
  resultType <- analyse e2 (TypeSubst.apply subst $ Assumptions.addPoly vn localType' assump)
  return resultType
analyse (Cond e1 e2 e3) assump = do
  testType <- analyse e1 assump
  thenType <- analyse e2 assump
  elseType <- analyse e3 assump
  unify testType boolType
  unify thenType elseType
  return thenType
analyse (Tuple es)      assump = do
  types <- mapM (\e -> analyse e assump) es
  return (tupleType types)

newTypeInstance :: VarName -> Assumptions -> Mon Type
newTypeInstance vn assump = do
  case Assumptions.lookup vn assump of
    Nothing -> fail ("Free variable "++ vn)
    Just (Forall tvns t) -> do
      tvs' <- mapM (const newTypeVar) tvns
      let subst = foldr (TypeSubst.compose . uncurry TypeSubst.singleton)
                        TypeSubst.empty (zip tvns tvs')
      return (TypeSubst.apply subst t)

generalize :: Type -> Assumptions -> Mon PolyType
generalize t assump = do
  currSubst <- getCurrentSubst
  let assump' = TypeSubst.apply currSubst assump
  let vars = freeTypeVars t `Set.difference` freeTypeVars assump'
  return (Forall (Set.toList vars) t)

typeOf :: Expr -> Type
typeOf expr = runMon $ do
  t <- analyse expr (Assumptions.empty)
  subst <- getCurrentSubst
  return (TypeSubst.apply subst t)

-- Examples
term1 = ConstB True
term2 = ConstI 2
term3 = App term1 term2
term4 = Abs "x" (Var "x")
term5 = App term4 term1
term6 = App term4 term4
term7 = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term8 = Cond term1 term4 term6
term9 = Cond term1 term1 term2
term10 = Abs "f" (Cond term1 (App (Var "f") term1) (App (Var "f") term2))
term11 = Abs "f" (Abs "g" (Tuple [App (Var "f") term1, App (Var "g") term2]))
term12 = Abs "x" $ Abs "y" $ Abs "z" (Cond (Var "x") (Var "y") (Var "x"))
term13 = Abs "x" $ Abs "y" $ Abs "z" $ Tuple [App (Var "y") (Var "x"), Var "x", Var "y", Var "z"]

term14 = Abs "f"  $ Abs "x" $ Cond (ConstB True) (Var "f") (App (Var "f") (Var "x"))
term15 = Abs "f"  $ Abs "x" $ Tuple [ App (Var "f") (Var "x"), App (Var "f") (App (Var "f") (Var "x")) ]
term16 = Abs "f"  $ Abs "x" $ App (Var "f") (App (Var "f") (Var "x"))
term17 = Abs "f" $ Tuple [ App (Var "f") (ConstB True), App (Var "f") (ConstI 2) ]
term18 = Let "f" (Abs "x" (Var "x")) (Tuple [Var "f", App (Var "f") (ConstB True), App (Var "f") (ConstI 1) ] )
term19 = Abs "f"  (Tuple [Var "f", App (Var "f") (ConstB True), App (Var "f") (ConstI 1) ] )

term20 = Abs "f" $ Abs "x" $ Let "v" (App (Var "f") (Var "x")) (Var "v")

-- Wrong
term21 = Abs "f" $ Abs "x" $ Let "v" (App (Var "f") (App (Var "f") (Var "x"))) (Var "v")
