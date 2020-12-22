{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Trans.Cont.Linear
  ( Cont
  , cont
  , runCont
  , ContT (..)
  ) where

import Data.Functor.Identity (Identity (Identity))

import qualified Prelude.Linear as PL
import qualified Data.Functor.Linear as FL
import qualified Control.Monad.Linear as ML

type K r m a = a #-> m r

type C r m a = K r m a #-> m r

data ContT r m a = ContT { runContT :: C r m a }

type Cont r a = ContT r Identity a

cont :: ((a #-> r) #-> r) #-> Cont r a
cont c =
  ContT (c' c)
  where
    c' :: ((a #-> r) #-> r) #-> C r Identity a
    c' c k =
      Identity (c (k' k))
      where
        k' :: (a #-> Identity r) #-> a #-> r
        k' k a = k a PL.& \case (Identity r) -> r

runCont :: forall r a. Cont r a -> (a #-> r) #-> r
runCont (ContT c) k =
  c (k' k) PL.& \case (Identity r) -> r
  where
    k' :: (a #-> r) #-> a #-> Identity r
    k' k a = Identity (k a)

instance FL.Functor (ContT r m) where
  fmap f c = ML.fmap f c

instance FL.Applicative (ContT r m) where
  pure x = ML.pure x

  cf <*> cv = cf ML.<*> cv

instance ML.Functor (ContT r m) where
  fmap f (ContT c) =
    ContT (c' f c)
    where
      c' :: (a #-> b) #-> C r m a #-> C r m b
      c' f c k = c (k PL.. f)

instance ML.Applicative (ContT r m) where
  pure x = ContT (\k -> k x)

  (ContT cf) <*> (ContT cv) =
    ContT (c cf cv)
    where
      c :: C r m (a #-> b) #-> C r m a #-> C r m b
      c cf cv k = cf (k' cv k)
      k' :: C r m a #-> K r m b #-> K r m (a #-> b)
      k' cv k f = cv (k PL.. f)

instance ML.Monad (ContT r m) where
  (ContT c) >>= f =
    ContT (c' c f)
    where
      c' :: C r m a #-> (a #-> ContT r m b) #-> C r m b
      c' c f k = c (k' f k)
      k' :: (a #-> ContT r m b) #-> K r m b #-> K r m a
      k' f k a = f a PL.& \case (ContT c) -> c k

instance ML.MonadFail (ContT r m) where
  fail = error
