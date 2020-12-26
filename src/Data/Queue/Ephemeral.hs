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
empty f = f (Queue [] [])

null :: Queue a #-> ((Bool, Queue a) #-> b) #-> b
null (Queue l m) f = f (P.null l, Queue l m)

enqueue :: a -> Queue a #-> (Queue a #-> b) #-> b
enqueue a (Queue l m) f = f (check l (a:m))

dequeue :: Queue a #-> (Maybe (a, Queue a) #-> b) #-> b
dequeue (Queue (a:l) m) f = f (Just (a, check l m))
dequeue (Queue _ _) f = f Nothing

check :: [a] -> [a] -> Queue a
check [] m = Queue (reverse m) []
check l m = Queue l m
