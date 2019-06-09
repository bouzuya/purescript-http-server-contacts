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
import Data.String as String
import Effect.Aff (Aff)
import ResponseHelper as ResponseHelper
import Simple.JSON as SimpleJSON
import Store as Store
import Type (AppStore, Contact)

execute :: AppStore -> Request -> Aff Response
execute store { method, pathname, body } = do
  let
    path =
      Array.filter
        (not <<< String.null)
        (String.split (String.Pattern "/") pathname)
  case method, path of
    Method.GET, ["contacts"] -> do
      contacts <- Store.get store
      ResponseHelper.json (SimpleJSON.writeJSON contacts)
    Method.POST, ["contacts"] -> do
      case (SimpleJSON.readJSON_ body :: _ Contact) of
        Maybe.Nothing ->
          ResponseHelper.json'
            StatusCode.status400
            (SimpleJSON.writeJSON { message: "bad request" })
        Maybe.Just contact -> do
          contacts <- Store.get store
          let contacts' = Array.insert contact contacts
          _ <- Store.put contacts' store
          ResponseHelper.json (SimpleJSON.writeJSON contacts)
    Method.GET, [] ->
      -- healthcheck
      ResponseHelper.json (SimpleJSON.writeJSON { message: "OK" })
    _, _ ->
      ResponseHelper.json'
        StatusCode.status404
        (SimpleJSON.writeJSON { message: "Not Found" })
