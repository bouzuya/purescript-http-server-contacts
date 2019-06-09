module Main (main) where

import Prelude

import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server as Server
import Bouzuya.HTTP.StatusCode (StatusCode)
import Bouzuya.HTTP.StatusCode as StatusCode
import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Console as Console
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.Process as Process
import Simple.JSON as SimpleJSON
import Store (Store)
import Store as Store

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type AppStore = Store (Array Contact)

html :: String -> Aff Response
html = html' StatusCode.status200

html' :: StatusCode -> String -> Aff Response
html' status text = do
  body <- Class.liftEffect (stringToUint8Array text)
  pure
    { body
    , headers: [ Tuple.Tuple "Content-Type" "text/html" ]
    , status
    }

stringToUint8Array :: String -> Effect Uint8Array
stringToUint8Array s = do
  b <- Buffer.fromString s Encoding.UTF8
  ab <- Buffer.toArrayBuffer b
  TypedArray.whole ab

app :: AppStore -> Request -> Aff Response
app store { method, pathname } = do
  let
    path =
      Array.filter
        (not <<< String.null)
        (String.split (String.Pattern "/") pathname)
  case method, path of
    Method.GET, ["contacts"] -> do
      contacts <- Store.get store
      html (String.joinWith "\n" (map SimpleJSON.writeJSON contacts))
    Method.GET, [] ->
      html (show method <> " " <> pathname)
    _, _ ->
      html' StatusCode.status404 "Not Found"

readPort :: Int -> Effect Int
readPort defaultPort =
  map
    (Maybe.fromMaybe defaultPort)
    (map (bindFlipped Int.fromString) (Process.lookupEnv "PORT"))

initialContacts :: Array Contact
initialContacts = [
  { name: "JR 三ノ宮駅"
  , address: "神戸市中央区布引町四丁目1-1"
  , tel: "999-999-9999"
  }
]

main :: Effect Unit
main = Aff.launchAff_ do
  store <- Store.new initialContacts
  port <- Class.liftEffect (readPort 8080)
  let config = { hostname: "0.0.0.0", port }
  Class.liftEffect (Server.run config (Console.log "listen") (app store))
