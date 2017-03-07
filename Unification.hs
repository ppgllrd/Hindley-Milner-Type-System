--------------------------------------------------------------------------------
-- Functions related to type unification
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module Unification(unify) where

import TypeVarName
import Type
import Mon
import TypeSubst(TypeSubst)
import qualified TypeSubst

-- Most general unifier
mgu :: Type -> Type -> Mon TypeSubst
mgu t1@(TypeVar tvn) t2
  | occurs tvn t2 = if isTypeVar t2 then return TypeSubst.empty
                                    else infiniteUnif t1 t2
  | otherwise     = return (TypeSubst.singleton tvn t2)
mgu t1 t2@(TypeVar tvn) = mgu t2 t1
mgu t1@(TypeOp op1 ts1) t2@(TypeOp op2 ts2)
  | op1 == op2 = mguAll ts1 ts2
  | otherwise  = typeMismatch t1 t2

mguAll :: [Type] -> [Type] -> Mon TypeSubst
mguAll []       []       = return TypeSubst.empty
mguAll (t1:ts1) (t2:ts2) = do
  subst1 <- mgu t1 t2
  let ts1' = TypeSubst.apply subst1 ts1
  let ts2' = TypeSubst.apply subst1 ts2
  subst2 <- mguAll ts1' ts2'
  return (subst2 `TypeSubst.compose` subst1)

unify :: Type -> Type -> Mon TypeSubst
unify t1 t2 = do
  currSubst <- getCurrentSubst
  let t1' = TypeSubst.apply currSubst t1
  let t2' = TypeSubst.apply currSubst t2
  newSubst <- mgu t1' t2'
  setCurrentSubst (newSubst `TypeSubst.compose` currSubst)
  return newSubst

-- Posible errors during unification
infiniteUnif :: Type -> Type -> Mon TypeSubst
infiniteUnif t1 t2 =
  fail (unlines [ "Type unification failed for:"
                , show t1
                , show t2
                , "Because of an infinite type being needed"
                ])

typeMismatch :: Type -> Type -> Mon TypeSubst
typeMismatch t1 t2 =
  fail (unlines [ "Type Unification failed for:"
                , show t1
                , show t2
                , "Because of type mismatch"
                ])
{-

-- Some tests
test ej = runMon ej

ej1 = test (mgu (alpha *-> alpha) (beta *-> beta))
  where alpha = TypeVar (typeVarName 0)
        beta  = TypeVar (typeVarName 1)

ej2 = test (mgu (alpha *-> alpha) (alpha))
  where alpha = TypeVar (typeVarName 0)

ej3 = test (mgu boolType (alpha *-> alpha))
  where alpha = TypeVar (typeVarName 0)

-}
