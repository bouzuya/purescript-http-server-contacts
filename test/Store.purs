module Test.Store
  ( tests
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import Store (Store)
import Store as Store
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Store" do
  let
    a1 = { key: "abc" }
    a2 = { key: "def" }
    a3 = { key: "ghi" }
    build xs = do
      store1 <- Store.empty
      Foldable.for_ xs \x -> do
        Store.insert store1 x.key x
      pure store1
    assertEqStore :: forall a. Eq a => Show a => Store a -> Store a -> Aff Unit
    assertEqStore s1 s2 = do
      l1 <- Store.list s1
      l2 <- Store.list s2
      Assert.equal l1 l2

  TestUnit.test "delete" do
    emptyStore <- Store.empty
    store1 <- build [a1]
    Store.delete store1 a1.key
    assertEqStore store1 emptyStore

  TestUnit.test "empty" do
    emptyStore <- Store.empty
    store1 <- build []
    assertEqStore store1 emptyStore

  TestUnit.test "get" do
    store1 <- build [a1]
    a1Maybe <- Store.get store1 a1.key
    a2Maybe <- Store.get store1 a2.key
    Assert.equal (Maybe.Just a1) a1Maybe
    Assert.equal Maybe.Nothing a2Maybe

  TestUnit.test "insert" do
    store1 <- Store.empty
    Store.insert store1 a1.key a1
    store2 <- build [a1]
    assertEqStore store1 store2

  TestUnit.test "list" do
    store1 <- build [a1, a2]
    as1 <- Store.list store1
    Assert.equal [a1, a2] as1

  TestUnit.test "update" do
    store1 <- build [a1, a2]
    Store.update store1 a1.key a3
    as1 <- Store.list store1
    Assert.equal [a3, a2] as1
