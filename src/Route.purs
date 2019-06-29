module Route
  ( route
  ) where

import Action (Action)
import Action as Action
import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath (NormalizedPath)
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath

route :: NormalizedPath -> Method -> Action
route normalizedPath method =
  case NormalizedPath.toPieces normalizedPath of
    ["contacts"] ->
      case method of
        Method.GET -> Action.ContactList
        Method.POST -> Action.ContactCreate
        _ -> Action.MethodNotAllowed [Method.GET, Method.POST]
    [] ->
      case method of
        Method.GET -> Action.HealthCheck
        _ -> Action.MethodNotAllowed [Method.GET]
    _ -> Action.NotFound
