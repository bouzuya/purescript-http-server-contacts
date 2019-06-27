module App
  ( execute
  ) where

import Prelude

import Action as Action
import Bouzuya.HTTP.Body as Body
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.Array as Array
import Data.Maybe as Maybe
import Effect.Class as Class
import Middleware.Logging as MiddlewareLogging
import Middleware.PathNormalize as MiddlewarePathNormalize
import Middleware.Routing as MiddlewareRouting
import ResponseHelper as ResponseHelper
import Simple.JSON as SimpleJSON
import Store as Store
import Type (AppStore, Middleware, NewHandler, Contact)

type R1 r = (store :: AppStore | r)
type R2 r = MiddlewareRouting.R Action.Action r

execute :: NewHandler (R1 ())
execute = middleware handler

handler :: NewHandler (R2 (R1 ()))
handler { action, request: { body }, store } =
  -- ResponseHelper.fromStatus StatusCode.status500 []
  case action of
    Action.ContactCreate -> do
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
    Action.ContactList -> do
      contacts <- Store.get store
      ResponseHelper.fromJSON contacts
    Action.HealthCheck ->
      ResponseHelper.fromStatus StatusCode.status200 []
    Action.MethodNotAllowed methods ->
      ResponseHelper.status405 methods
    Action.NotFound ->
      ResponseHelper.status404

middleware :: Middleware (R1 ()) (R2 (R1 ()))
middleware =
  (MiddlewareRouting.middleware Action.route)
  >>> MiddlewareLogging.middleware
  >>> MiddlewarePathNormalize.middleware
