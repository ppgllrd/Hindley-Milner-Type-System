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
  , fromChar
  , next
  ) where

import Data.Char(ord, chr)

-- Type Variables
newtype TypeVarName = MkTypeVarName Int deriving (Eq, Ord)

fromInt :: Int -> TypeVarName
fromInt = MkTypeVarName

offset = 100 :: Int

fromChar :: Char -> TypeVarName
fromChar c = MkTypeVarName (ord c - offset)

next :: TypeVarName -> TypeVarName
next (MkTypeVarName tvn) = MkTypeVarName (tvn + 1)

instance Show TypeVarName where
  showsPrec p (MkTypeVarName tv)
    | tv < 0    = showChar (chr (offset + tv))
    | otherwise = showString "t" . shows tv
