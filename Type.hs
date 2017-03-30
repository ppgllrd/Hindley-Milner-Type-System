--------------------------------------------------------------------------------
-- Functions and types related to type expresions
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
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
  , listType
  ) where

import Data.List(intersperse)
import qualified Data.Set as Set
import Lex
import TypeVarName
import FreeTypeVars

-- The language of Type Expresions
data Type = TypeVar TypeVarName
          | TypeOp String [ Type ]

data PolyType = Forall [TypeVarName] Type

-- Some standard types
boolType :: Type
boolType = TypeOp lxBool []

intType :: Type
intType = TypeOp lxInt []

(*->) :: Type -> Type -> Type
t1 *-> t2 = TypeOp lxArrow [t1, t2]

tupleType :: [Type] -> Type
tupleType ts = TypeOp lxTuple ts

listType :: Type -> Type
listType t = TypeOp lxList [t]

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
    | op == lxArrow = let [t1,t2] = ts
                      in showParen (p>9) ( showsPrec 10 t1
                                         . showChar ' ' . showString lxArrow . showChar ' '
                                         . showsPrec p t2
                                         )
    | op == lxTuple =   showString lxOP
                      . foldl (.) id (intersperse (showString lxComma . showChar ' ')
                                        [ showsPrec 0 t | t <- ts ])
                      . showString lxCP
    | op == lxList =  let [t] = ts
                      in   showString lxOL
                         . showsPrec 0 t
                         . showString lxCL
    | otherwise     = showParen (p>9) ( showString op
                                      . foldl (.) id [ showChar ' ' . showsPrec 10 t | t <- ts ]
                                      )
   showsPrec p _  = error "unknown pattern in showsPrec for Type"

instance Show PolyType where
  showsPrec p (Forall [] t) =
     showsPrec 0 t
  showsPrec p (Forall tvns t) =
    showParen (p>9) (
        foldl1 (.) (intersperse (showChar '.') [ showString lxForall . showsPrec 0 tvn | tvn <- tvns ])
      . showString ". "
      . showsPrec 0 t
      )
