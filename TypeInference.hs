--------------------------------------------------------------------------------
-- The type analyser
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
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

analyse :: Expr -> Mon Type
analyse (ConstB bool) =
  return boolType
analyse (ConstI int) =
  return intType
analyse (Var vnm) =
  newTypeInstance vnm
analyse (App e1 e2) = do
  funType <- analyse e1
  argType <- analyse e2
  resType <- newTypeVar
  unify (argType *-> resType) funType
  return resType
analyse (Abs vn e) = do
  argType <- newTypeVar
  resType <- analyse e `withModifiedAssumptions` Assumptions.add vn argType
  return (argType *-> resType)
analyse (Let vn e1 e2) = do
  localType <- analyse e1
  localType' <- generalize localType
  resultType <- analyse e2 `withModifiedAssumptions` Assumptions.addPoly vn localType'
  return resultType
analyse (Cond e1 e2 e3) = do
  testType <- analyse e1
  thenType <- analyse e2
  elseType <- analyse e3
  unify testType boolType
  unify thenType elseType
  return thenType
analyse (Tuple es) = do
  types <- mapM analyse es
  return (tupleType types)

newTypeInstance :: VarName -> Mon Type
newTypeInstance vn = do
  assump <- getAssumptions
  case Assumptions.lookup vn assump of
    Nothing -> fail ("Free variable "++ vn)
    Just (Forall tvns t) -> do
      tvs' <- mapM (const newTypeVar) tvns
      let subst = foldr (TypeSubst.compose . uncurry TypeSubst.singleton)
                        TypeSubst.empty (zip tvns tvs')
      return (TypeSubst.apply subst t)

generalize :: Type -> Mon PolyType
generalize t = do
  assump <- getAssumptions
  currSubst <- getCurrentSubst
  let assump' = TypeSubst.apply currSubst assump
  let vars = freeTypeVars t `Set.difference` freeTypeVars assump'
  return (Forall (Set.toList vars) t)

typeOf :: Expr -> Type
typeOf expr = runMon Assumptions.empty $ do
  t <- analyse expr
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
