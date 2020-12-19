{-# LANGUAGE Strict #-}
{-# LANGUAGE LinearTypes #-}

module Data.Queue.Ephemeral
  ( Queue ()
  , empty
  , null
  , enqueue
  , dequeue
  , remove
  ) where

import Prelude hiding (null)
import qualified Prelude
import qualified Prelude.Linear as LP

data Queue a =
  Queue [a] [a]
  deriving (Show, Eq, Ord)

empty :: (Queue a #-> b) -> b
empty f = f LP.$ Queue [] []

null :: Queue a -> Bool
null (Queue l _) = Prelude.null l

enqueue :: a -> Queue a -> (Queue a #-> b) -> b
enqueue a (Queue l m) f = f LP.$ check l $ a:m

dequeue :: Queue a -> Maybe a
dequeue (Queue (a:_) _) = Just a
dequeue _ = Nothing

remove :: Queue a -> (Maybe (Queue a) #-> b) -> b
remove (Queue (_:l) m) f = f LP.$ Just $ check l m
remove _ f = f Nothing

check :: [a] -> [a] -> Queue a
check [] m = Queue (reverse m) []
check l m = Queue l m
