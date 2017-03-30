--------------------------------------------------------------------------------
-- Lexical elements of the language
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module Lex
  ( lxTrue
	, lxFalse

	, lxIf
	, lxThen
	, lxElse
	, lxLet
	, lxLetrec
	, lxIn

	, lxOP
	, lxCP
	, lxOL
	, lxCL
  , lxOB
	, lxCB
	, lxComma
	, lxUnder

	, lxEq
	, lxArrow
	, lxLambda
  , lxSlash
  , lxForall

	, lxBool
	, lxInt
	, lxTuple
  , lxEmptyTuple
	, lxEmptyList

  , lxList
	, lxCons
	, lxNil
	, lxInfixCons

	, lxPlus
  ) where

lxTrue   = "True"
lxFalse  = "False"

lxIf     = "if"
lxThen   = "then"
lxElse   = "else"

lxLet    = "let"
lxLetrec = "letrec"
lxIn     = "in"

lxOP     = "("
lxCP     = ")"
lxOL     = "["
lxCL     = "]"
lxOB     = "{"
lxCB     = "}"
lxComma  = ","
lxUnder  = "_"

lxEq	 = "="
lxArrow  = "->"
lxLambda = "\\"
lxSlash  = "/"
lxForall = "\\/"

lxBool       = "Bool"
lxInt        = "Int"
lxTuple      = "*"

lxEmptyTuple = lxOP++lxCP
lxEmptyList  = lxOL++lxCL

lxList       = "List"
lxCons       = "_Cons"
lxNil        = "_Nil"
lxInfixCons  = ":"

lxPlus       = "plus" --ToDo: use +
