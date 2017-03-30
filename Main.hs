--------------------------------------------------------------------------------
-- Free type variables
--
-- Main program
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

import qualified Assumptions
import Expr
import ExprParser
import Lex
import Mon
import Type
import TypeInference
import qualified TypeVarName
import qualified TypeVarName

assumptions :: Assumptions.Assumptions
assumptions =
  Assumptions.addPoly lxNil (Forall [alphaName] listAlpha) $
  Assumptions.addPoly lxCons (Forall [alphaName] $ alpha *-> (listAlpha *-> listAlpha)) $
  Assumptions.empty
  where
    alphaName = TypeVarName.fromChar 'a'
    alpha = TypeVar alphaName
    listAlpha = listType alpha

typeOfString :: String -> Type
typeOfString str = runMon assumptions $ do
  expr <- parseExpr str
  typeOf expr

main :: IO ()
main =
  mapM_ (\ex -> do putStr ex; putStr " :: "; print (typeOfString ex)) examples

examples =
  [ "\\x -> x"
  , "\\f x -> (f, f x, f (f x))"
  , "\\x -> [x, x]"
  , "let f = \\x -> x in (f, f True, f 1)"
  , "\\f x -> if True then f 2 else f x"
  ]
