module Action
  ( Action(..)
  , route
  ) where

import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath (NormalizedPath)
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath

data Action
  = ContactCreate
  | ContactList
  | HealthCheck
  | MethodNotAllowed (Array Method)
  | NotFound

route :: NormalizedPath -> Method -> Action
route normalizedPath method =
  case NormalizedPath.toPieces normalizedPath of
    ["contacts"] ->
      case method of
        Method.GET -> ContactList
        Method.POST -> ContactCreate
        _ -> MethodNotAllowed [Method.GET, Method.POST]
    [] ->
      case method of
        Method.GET -> HealthCheck
        _ -> MethodNotAllowed [Method.GET]
    _ -> NotFound
