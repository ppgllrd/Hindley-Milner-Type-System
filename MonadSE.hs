--------------------------------------------------------------------------------
-- A generic Monad with error and state
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, December 1998
--
--------------------------------------------------------------------------------

module MonadSE
  ( MSE
  , ErrorMsg
  , runMSE
  , writeST
  , readST
  ) where

import Control.Monad

-------------------------------------------------------
-- The monad
-------------------------------------------------------

type ErrorMsg = String

newtype MSE s a = MkMSE (s -> Either ErrorMsg (s, a))

instance Monad (MSE s) where
  -- return :: a -> MSE s a
  return x = MkMSE (\s -> Right (s, x))

  -- (>>=) :: MSE s a -> (a -> MSE s b) -> MSE s b
  MkMSE st >>= f  = MkMSE (\s -> case st s of
                                   Left err      -> Left err
                                   Right (s', x) -> let MkMSE st' = f x
                                                    in st' s')

  -- fail :: ErrorMsg -> MSE s a
  fail err = MkMSE (\s -> Left err)

runMSE :: MSE s a -> s -> (s, a)
runMSE (MkMSE st) s =
  case st s of
    Left err      -> error ('\n':err)
    Right (s', x) -> (s', x)


instance MonadPlus (MSE s) where
  -- mzero :: MSE s a
  mzero = fail "MSE zero"

  -- mplus :: MSE s a -> MSE s a -> MSE s a
  mplus (MkMSE st1) (MkMSE st2) =
    MkMSE (\s -> case st1 s of
                  right@(Right (s', x)) -> right
                  Left err              -> st2 s)

updateST :: (s -> s) -> MSE s s
updateST f = MkMSE (\s -> Right (f s, s))

readST :: MSE s s
readST = updateST id

writeST :: s -> MSE s ()
writeST s = do
  updateST (\_ -> s)
  return ()
