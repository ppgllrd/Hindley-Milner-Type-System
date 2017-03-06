--------------------------------------------------------------------------------
-- Type variable names
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module TypeVarName
  ( TypeVarName
  , typeVarName
  , nextTypeVarName
  ) where

-- Type Variables
newtype TypeVarName = MkTypeVarName Int deriving (Eq, Ord)

typeVarName :: Int -> TypeVarName
typeVarName = MkTypeVarName

nextTypeVarName :: TypeVarName -> TypeVarName
nextTypeVarName (MkTypeVarName tvn) = MkTypeVarName (tvn + 1)

instance Show TypeVarName where
  showsPrec p (MkTypeVarName tv) = showString "t" . shows tv
