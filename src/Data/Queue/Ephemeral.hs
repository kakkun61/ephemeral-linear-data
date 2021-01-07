{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE Strict #-}

module Data.Queue.Ephemeral
  ( Queue ()
  , empty
  , null
  , enqueue
  , dequeue
  ) where

import qualified Prelude as P
import Prelude hiding (null)
import qualified Prelude.Linear as PL
import Prelude.Linear (Consumable, Ur (Ur), lseq)

data Queue a where
  Queue :: [a] -> [a] -> Queue a
  deriving (Show)

instance Consumable a => Consumable (Queue a) where
  consume (Queue l m) = l `lseq` m `lseq` ()

empty :: (Queue a %1 -> Ur b) %1 -> Ur b
empty k = k (Queue [] [])

null :: Queue a %1 -> (Ur Bool, Queue a)
null (Queue l m) = (Ur (P.null l), Queue l m)

enqueue :: a -> Queue a %1 -> Queue a
enqueue a (Queue l m) = check l (a:m)

dequeue :: Queue a %1 -> (Ur (Maybe a), Queue a)
dequeue (Queue (a:l) m) = (Ur (Just a), check l m)
dequeue (Queue l m) = (Ur Nothing, Queue l m)

check :: [a] -> [a] -> Queue a
check [] m = Queue (reverse m) []
check l m = Queue l m
