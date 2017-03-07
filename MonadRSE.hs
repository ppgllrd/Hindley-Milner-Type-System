--------------------------------------------------------------------------------
-- A generic reader Monad with error and state
--
-- A type inference system for a small functional language
--
-- Pepe Gallardo, 2017
--
--------------------------------------------------------------------------------

module MonadRSE
  ( MRSE
  , ErrorMsg
  , runMRSE
  , writeST
  , readST
  , reader
  , withReader
  ) where

import Control.Monad

-------------------------------------------------------
-- The monad
-------------------------------------------------------

type ErrorMsg = String

newtype MRSE r s a = MkMRSE (r -> s -> Either ErrorMsg (s, a))

instance Monad (MRSE r s) where
  -- return :: a -> MRSE r s a
  return x = MkMRSE (\r s -> Right (s, x))

  -- (>>=) :: MRSE r s a -> (a -> MRSE r s b) -> MRSE r s b
  MkMRSE st >>= f  = MkMRSE (\r s -> case st r s of
                                   Left err      -> Left err
                                   Right (s', x) -> let MkMRSE st' = f x
                                                    in st' r s')

  -- fail :: ErrorMsg -> MRSE s a
  fail err = MkMRSE (\r s -> Left err)

runMRSE :: MRSE r s a -> r -> s -> (s, a)
runMRSE (MkMRSE st) r s =
  case st r s of
    Left err      -> error ('\n':err)
    Right (s', x) -> (s', x)

instance MonadPlus (MRSE r s) where
  -- mzero :: MRSE r s a
  mzero = fail "MRSE zero"

  -- mplus :: MRSE r s a -> MRSE r s a -> MRSE r s a
  mplus (MkMRSE st1) (MkMRSE st2) =
    MkMRSE (\r s -> case st1 r s of
                  right@(Right (s', x)) -> right
                  Left err              -> st2 r s)

updateST :: (s -> s) -> MRSE r s s
updateST f = MkMRSE (\r s -> Right (f s, s))

readST :: MRSE r s s
readST = updateST id

writeST :: s -> MRSE r s ()
writeST s = do
  updateST (\_ -> s)
  return ()

reader :: MRSE r s r
reader = MkMRSE (\r s -> Right (s, r))

withReader :: MRSE r s a -> r -> MRSE r s a
(MkMRSE st1) `withReader` r = MkMRSE (\_ s -> st1 r s)
