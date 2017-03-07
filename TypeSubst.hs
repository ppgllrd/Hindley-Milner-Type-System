--------------------------------------------------------------------------------
-- Functions related to type substitutions
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module TypeSubst
  ( TypeSubst
  , empty
  , singleton
  , Substitutable
  , apply
  , compose
  ) where

import Data.List(intersperse)
import Type
import TypeVarName

-- A type substitution maps names of type variables to type expressions
data SingleTypeSubst = Type :/ TypeVarName

instance Show SingleTypeSubst where
  showsPrec p (t :/ tvName) =
    showParen (p>9) ( shows t
                    . showString "/"
                    . shows tvName
                    )

-- Type substitutions: to be performed in right to left order
-- [Sigma1/a1, ..., Sigman/an]

newtype TypeSubst = MkTS [SingleTypeSubst]

instance Show TypeSubst where
  showsPrec p (MkTS ss) =
    showParen (p>9) ( showString "{"
                    . foldl1 (.) (intersperse (showString ", ") (map (showsPrec 0) ss))
                    . showString "}"
                    )

-- The id substitution. Maps each type variable to itself:
--   forall t . apply emptyTypeSubst t == t
empty :: TypeSubst
empty = MkTS []

-- When applied, replaces tvn with t
singleton :: TypeVarName -> Type -> TypeSubst
singleton tvn t = MkTS [t :/ tvn]

class Substitutable a where
  apply :: TypeSubst -> a -> a

instance (Substitutable a) => Substitutable [a] where
  apply s = map (apply s)

-- Applies type susbtitutions to a type:
--
-- Definition:
--
-- Let S = [Sigma1/a1, ..., Sigman/an]
--
-- then
--
-- (i)   S(ai)           = Sigmai
-- (ii)  S(op [a,b....]) = op [S(a), S(b) ...]
-- (iii) S(b)            = b, otherwise

instance Substitutable Type where
  apply (MkTS ss) t = foldr applyATypeSubst t ss
    where
     applyATypeSubst (sigma:/a) tv'@(TypeVar a')
       | a == a'                               = sigma
       | otherwise                             = tv'
     applyATypeSubst s          (TypeOp op ts) = TypeOp op (map (applyATypeSubst s) ts)
     applyATypeSubst _          _              = error "unknown pattern in applyATypeSubst"

instance Substitutable PolyType where
  apply subst (Forall tvns t) = Forall tvns (apply subst t)

-- Composes two type substitutions: ss2 is performed after ss1
compose :: TypeSubst -> TypeSubst -> TypeSubst
compose s2@(MkTS ss2) (MkTS ss1) =
  MkTS ([ apply s2 t :/ vn | t :/ vn <- ss1 ] ++ ss2)
