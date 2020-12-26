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
import Prelude.Linear (Consumable, lseq)

data Queue a where
  Queue :: [a] -> [a] -> Queue a
  deriving (Show)

instance Consumable a => Consumable (Queue a) where
  consume (Queue l m) = l `lseq` m `lseq` ()

empty :: (Queue a #-> b) #-> b
empty k = k (Queue [] [])

null :: Queue a #-> ((Bool, Queue a) #-> b) #-> b
null (Queue l m) k = k (P.null l, Queue l m)

enqueue :: a -> Queue a #-> (Queue a #-> b) #-> b
enqueue a (Queue l m) k = k (check l (a:m))

dequeue :: Queue a #-> (Maybe (a, Queue a) #-> b) #-> b
dequeue (Queue (a:l) m) k = k (Just (a, check l m))
dequeue (Queue _ _) k = k Nothing

check :: [a] -> [a] -> Queue a
check [] m = Queue (reverse m) []
check l m = Queue l m
