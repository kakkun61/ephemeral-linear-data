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
import Prelude.Linear (Consumable, Dupable, Movable, Ur (Ur), lseq)

data Queue a where
  Queue :: [a] -> [a] -> Queue a
  deriving (Show, Eq, Ord)

instance Consumable a => Consumable (Queue a) where
  consume (Queue l m) = l `lseq` m `lseq` ()

instance Consumable a => Dupable (Queue a) where
  dup2 (Queue l m) = let q = Queue l m in (q, q)

instance Consumable a => Movable (Queue a) where
  move (Queue l m) = Ur (Queue l m)

empty :: (Queue a #-> b) #-> b
empty f = f (Queue [] [])

null :: Queue a #-> Bool
null (Queue l _) = P.null l

enqueue :: a -> Queue a #-> (Queue a #-> b) #-> b
enqueue a (Queue l m) f = f (check l (a:m))

dequeue :: Queue a #-> (Maybe (a, Queue a) #-> b) #-> b
dequeue (Queue (a:l) m) f = f (Just (a, check l m))
dequeue (Queue _ _) f = f Nothing

check :: [a] -> [a] -> Queue a
check [] m = Queue (reverse m) []
check l m = Queue l m
