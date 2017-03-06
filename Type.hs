--------------------------------------------------------------------------------
-- Functions and types related to type expresions
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module Type
  ( Type(..)
  , PolyType(..)
  , occurs
  , isTypeVar
  , (*->)
  , boolType
  , intType
  , tupleType
  ) where

import Data.List
import qualified Data.Set as Set
import TypeVarName
import FreeTypeVars

-- The language of Type Expresions
data Type = TypeVar TypeVarName
          | TypeOp String [ Type ]

data PolyType = Forall [TypeVarName] Type

-- Some standard types
tkBool  = "Bool"
tkInt   = "Int"
tkArrow = "->"
tkTuple = "*"

boolType :: Type
boolType = TypeOp tkBool []

intType :: Type
intType = TypeOp tkInt []

(*->) :: Type -> Type -> Type
t1 *-> t2 = TypeOp tkArrow [t1, t2]

tupleType :: [Type] -> Type
tupleType ts = TypeOp tkTuple ts

instance FreeTypeVars Type where
  freeTypeVars (TypeVar tvn) = Set.singleton tvn
  freeTypeVars (TypeOp op ts) = freeTypeVars ts

instance FreeTypeVars PolyType where
  freeTypeVars (Forall tvns t) = freeTypeVars t `Set.difference` Set.fromList tvns

-- Checks whether a type variable appears in a type expression
occurs :: TypeVarName -> Type -> Bool
occurs tvn (TypeVar tvn') = tvn == tvn'
occurs tvn (TypeOp op ts) = any (occurs tvn) ts

-- Checks whether a type expression is just a type variable
isTypeVar :: Type -> Bool
isTypeVar (TypeVar tvn) = True
isTypeVar _             = False

instance Show Type where
   showsPrec p (TypeVar tvn) = showsPrec p tvn
   showsPrec p (TypeOp op []) = showString op
   showsPrec p (TypeOp op ts)
    | op == tkArrow = let [t1,t2] = ts
                      in showParen (p>9) ( showsPrec 10 t1
                                         . showChar ' ' . showString tkArrow . showChar ' '
                                         . showsPrec p t2
                                         )
    | op == tkTuple = showParen True (foldl1 (.) (intersperse (showString ", ") (map (showsPrec 0) ts)))
    | otherwise     = showParen (p>9) ( showString op
                                      . showList ts
                                      )
   showsPrec p _  = error "unknown pattern in showsPrec for Type"

instance Show PolyType where
  showsPrec p (Forall [] t) =
     showsPrec 0 t
  showsPrec p (Forall tvns t) =
    showParen (p>9) (
        foldl1 (.) (intersperse (showChar '.') (map (\tvn -> showString "\\/" . showsPrec 0 tvn) tvns))
      . showString ". "
      . showsPrec 0 t
      )
