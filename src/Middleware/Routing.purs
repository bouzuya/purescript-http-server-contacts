module Middleware.Routing
  ( middleware
  ) where

import Prelude

import Bouzuya.HTTP.Body as Body
import Bouzuya.HTTP.Method as Method
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

middleware :: forall r r'. Middleware (R2 (R1 r)) r'
middleware
  _
  { normalizedPath
  , request: { method, body }
  , store
  } = do
  case NormalizedPath.toPieces normalizedPath of
    ["contacts"] ->
      case method of
        Method.GET -> do
          contacts <- Store.get store
          ResponseHelper.fromJSON contacts
        Method.POST -> do
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
        _ -> ResponseHelper.status405 [Method.GET, Method.POST]
    [] ->
      case method of
        Method.GET -> ResponseHelper.fromStatus StatusCode.status200 []
        _ -> ResponseHelper.status405 [Method.GET]
    _ -> ResponseHelper.status404
