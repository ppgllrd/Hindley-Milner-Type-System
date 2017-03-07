--------------------------------------------------------------------------------
-- Functions related to assumption sets
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module Assumptions
  ( Assumptions
  , empty
  , add
  , addPoly
  , lookup
  ) where

import Prelude hiding (lookup)
import qualified Data.Set as Set
import Type
import Expr
import FreeTypeVars
import TypeSubst(Substitutable, apply)

data Binding  = VarName :-> PolyType

newtype Assumptions = MkAs [Binding]

-- An empty assumption set
empty :: Assumptions
empty = MkAs []

-- Add a non generic assumption
add :: VarName -> Type -> Assumptions -> Assumptions
add vn t (MkAs bds) = MkAs ((vn :-> Forall [] t):bds)

-- Add a generic assumption
addPoly :: VarName -> PolyType -> Assumptions -> Assumptions
addPoly vn poly (MkAs bds) = MkAs ((vn :-> poly):bds)

-- Lookup type corresponding to variable
lookup :: VarName -> Assumptions -> Maybe PolyType
lookup vn (MkAs bds) =
  case [ poly | vn' :-> poly  <-  bds, vn'==vn ] of
    []       -> Nothing
    (poly:_) -> Just poly

instance FreeTypeVars Assumptions where
  freeTypeVars (MkAs bds) = Set.unions [ freeTypeVars poly | vn' :-> poly  <-  bds ]

instance Substitutable Assumptions where
  apply subst (MkAs bds) = MkAs [ vn :-> apply subst poly | vn :-> poly  <-  bds ]
