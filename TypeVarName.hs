--------------------------------------------------------------------------------
-- Type variable names
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module TypeVarName
  ( TypeVarName
  , fromInt
  , next
  ) where

-- Type Variables
newtype TypeVarName = MkTypeVarName Int deriving (Eq, Ord)

fromInt :: Int -> TypeVarName
fromInt = MkTypeVarName

next :: TypeVarName -> TypeVarName
next (MkTypeVarName tvn) = MkTypeVarName (tvn + 1)

instance Show TypeVarName where
  showsPrec p (MkTypeVarName tv) = showString "t" . shows tv
