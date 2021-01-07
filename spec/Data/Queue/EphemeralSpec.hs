{-# LANGUAGE LinearTypes #-}

module Data.Queue.EphemeralSpec (spec) where

import Data.Queue.Ephemeral

import Prelude hiding (null)
import Prelude.Linear (Ur (Ur), lseq)
import qualified Prelude.Linear as PL

import Test.Hspec

spec :: Spec
spec = do
  it "empty" $ do
    let
      f :: Queue () %1 -> Ur ()
      f q = q `lseq` Ur ()
      Ur a = empty f
    a `shouldBe` ()

  it "empty → null" $ do
    let
      f :: Queue () %1 -> Ur Bool
      f q =
        null q PL.& \(n, q) ->
        q `lseq` n
      Ur n = empty f
    n `shouldBe` True

  it "empty → enqueue" $ do
    let
      f :: Queue () %1 -> Ur ()
      f q =
        enqueue () q PL.& \q ->
        q `lseq` Ur ()
      Ur a = empty f
    a `shouldBe` ()

  it "empty → enqueue → dequeue" $ do
    let
      f :: Queue Int %1 -> Ur Int
      f q =
        enqueue 0 q PL.& \q ->
        dequeue q PL.& \(Ur (Just a), q) ->
        q `lseq` Ur a
      Ur a = empty f
    a `shouldBe` 0
