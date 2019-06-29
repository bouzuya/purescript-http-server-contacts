module Test.Route
  ( tests
  ) where

import Prelude

import Action as Action
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath (NormalizedPath)
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath
import Route as Route
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Route" do
  TestUnit.test "GET /contacts" do
    Assert.equal
      Action.ContactList
      (Route.route (NormalizedPath.normalize "/contacts") Method.GET)

  TestUnit.test "POST /contacts" do
    Assert.equal
      Action.ContactCreate
      (Route.route (NormalizedPath.normalize "/contacts") Method.POST)

  TestUnit.test "PATCH /contacts" do
    Assert.equal
      (Action.MethodNotAllowed [Method.GET, Method.POST])
      (Route.route (NormalizedPath.normalize "/contacts") Method.PATCH)

  TestUnit.test "GET /" do
    Assert.equal
      Action.HealthCheck
      (Route.route (NormalizedPath.normalize "/") Method.GET)

  TestUnit.test "PATCH /" do
    Assert.equal
      (Action.MethodNotAllowed [Method.GET])
      (Route.route (NormalizedPath.normalize "/") Method.PATCH)

  TestUnit.test "GET /foo" do
    Assert.equal
      Action.NotFound
      (Route.route (NormalizedPath.normalize "/foo") Method.GET)
