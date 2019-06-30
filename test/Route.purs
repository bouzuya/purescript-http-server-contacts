module Test.Route
  ( tests
  ) where

import Prelude

import Action as Action
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath
import Route as Route
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Route" do
  TestUnit.suite "/contacts" do
    let path = NormalizedPath.normalize "/contacts"

    TestUnit.test "GET" do
      Assert.equal Action.ContactList (Route.route path Method.GET)

    TestUnit.test "POST" do
      Assert.equal Action.ContactCreate (Route.route path Method.POST)

    TestUnit.test "PATCH" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET, Method.POST])
        (Route.route path Method.PATCH)

  TestUnit.suite "/" do
    let path = NormalizedPath.normalize "/"

    TestUnit.test "GET" do
      Assert.equal Action.HealthCheck (Route.route path Method.GET)

    TestUnit.test "PATCH" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET])
        (Route.route path Method.PATCH)

  TestUnit.suite "/foo" do
    let path = NormalizedPath.normalize "/foo"

    TestUnit.test "GET" do
      Assert.equal Action.NotFound (Route.route path Method.GET)
