module Middleware.Routing
  ( middleware
  ) where

import Prelude

import Bouzuya.HTTP.Body as Body
import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath (NormalizedPath)
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.Array as Array
import Data.Maybe as Maybe
import Effect.Class as Class
import Middleware.PathNormalize as MiddlewarePathNormalize
import ResponseHelper as ResponseHelper
import Simple.JSON as SimpleJSON
import Store as Store
import Type (Contact, Middleware, AppStore)

type R1 r = (store :: AppStore | r)
type R2 r = MiddlewarePathNormalize.R r

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

middleware :: forall r r'. Middleware (R2 (R1 r)) r'
middleware
  _
  { normalizedPath
  , request: { method, body }
  , store
  } =
    case route normalizedPath method of
      ContactCreate -> do
        body' <- Class.liftEffect (Body.fromArray body) -- TODO
        case (SimpleJSON.readJSON_ body' :: _ Contact) of
          Maybe.Nothing ->
            -- TODO: message
            ResponseHelper.fromStatus StatusCode.status400 []
          Maybe.Just contact -> do
            contacts <- Store.get store
            let contacts' = Array.insert contact contacts
            _ <- Store.put contacts' store
            ResponseHelper.fromJSON contacts
      ContactList -> do
        contacts <- Store.get store
        ResponseHelper.fromJSON contacts
      HealthCheck ->
        ResponseHelper.fromStatus StatusCode.status200 []
      MethodNotAllowed methods ->
        ResponseHelper.status405 methods
      NotFound ->
        ResponseHelper.status404
