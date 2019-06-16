module Action
  ( execute
  ) where

import Prelude

import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.Array as Array
import Data.Maybe as Maybe
import Middleware.PathNormalize as MiddlewarePathNormalize
import NormalizedPath as NormalizedPath
import ResponseHelper as ResponseHelper
import Simple.JSON as SimpleJSON
import Store as Store
import Type (AppStore, Contact, NewHandler)

type R1 r = (store :: AppStore | r)
type R2 r = MiddlewarePathNormalize.R r

execute :: NewHandler (R1 ())
execute = MiddlewarePathNormalize.middleware execute'

execute' :: NewHandler (R2 (R1 ()))
execute' { normalizedPath: normalized, request: { method, pathname, body }, store }  = do
  case NormalizedPath.toPieces normalized of
    ["contacts"] ->
      case method of
        Method.GET -> do
          contacts <- Store.get store
          ResponseHelper.fromJSON contacts
        Method.POST -> do
          case (SimpleJSON.readJSON_ body :: _ Contact) of
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
        Method.GET ->
          -- healthcheck
          ResponseHelper.fromJSON { message: "OK" }
        _ -> ResponseHelper.status405 [Method.GET]
    _ -> ResponseHelper.status404
