--------------------------------------------------------------------------------
-- A data type for expressions in the language
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module Expr
  ( VarName
  , Expr(..)
  ) where

import Data.List(intersperse)

-- Name of a variable
type VarName = String

data Expr = ConstB Bool           -- True, False
          | ConstI Int            -- ..., -1, 0, 1, ...
          | Var VarName           -- v, ...
          | App Expr Expr         -- e1 e2
          | Abs VarName Expr      -- \v -> e
          | Let VarName Expr Expr -- let v = e1 in e2
          | Cond Expr Expr Expr   -- if e1 then e2 else e3
          | Tuple [Expr]          -- (e1,e2), (e1,e2,e3) , ...

instance Show Expr where
  showsPrec p (ConstB b) =
    showsPrec p b
  showsPrec p (ConstI i) =
    showsPrec p i
  showsPrec p (Var vn) =
    showString vn
  showsPrec p (App e1 e2) =
    showParen (p>9) ( showsPrec 10 e1
                    . showChar ' '
                    . showsPrec 10 e2
                    )
  showsPrec p (Abs vn e) =
    showParen (p>9) ( showChar '\\'
                    . showString vn
                    . showString " -> "
                    . showsPrec p e
                    )
  showsPrec p (Let vn e1 e2) =
    showParen (p>9) ( showString "let "
                    . showString vn
                    . showString " = "
                    . showsPrec 0 e1
                    . showString " in "
                    . showsPrec 0 e2
                    )
  showsPrec p (Cond e1 e2 e3) =
    showParen (p>9) ( showString "if "
                    . showsPrec 0 e1
                    . showString " then "
                    . showsPrec 0 e2
                    . showString " else "
                    . showsPrec 0 e3
                    )
  showsPrec p (Tuple es) =
    showParen True (
        foldl1 (.) (intersperse (showString ", ") (map (showsPrec 0) es))
      )


-- Some examples
expr1 = Abs "x" (Var "x")

expr2 = App expr1 expr1

expr3 = Tuple [Var "x", ConstI 1, ConstB False]
