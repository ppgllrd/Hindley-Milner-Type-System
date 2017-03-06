--------------------------------------------------------------------------------
-- Free type variables
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module FreeTypeVars(FreeTypeVars, freeTypeVars) where

import Data.Set(Set)
import qualified Data.Set as Set
import TypeVarName

class FreeTypeVars a where
  freeTypeVars :: a -> Set TypeVarName

instance (FreeTypeVars a) => FreeTypeVars [a] where
  freeTypeVars = Set.unions . map freeTypeVars
