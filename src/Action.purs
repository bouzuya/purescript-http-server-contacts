module Action
  ( execute
  ) where

import Prelude

import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.Array as Array
import Data.Maybe as Maybe
import Effect.Aff (Aff)
import NormalizedPath as NormalizedPath
import ResponseHelper as ResponseHelper
import Simple.JSON as SimpleJSON
import Store as Store
import Type (AppStore, Contact)

execute :: AppStore -> Request -> Aff Response
execute store { method, pathname, body } = do
  let normalized = NormalizedPath.normalize pathname
  if pathname /= NormalizedPath.toString normalized
    then ResponseHelper.status301 (NormalizedPath.toString normalized)
    else
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
