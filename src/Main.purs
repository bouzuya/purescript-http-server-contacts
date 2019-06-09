module Main (main) where

import Prelude

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.Server as Server
import Bouzuya.HTTP.StatusCode as StatusCode
import Control.Bind (bindFlipped)
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Console as Console
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.Process as Process

html :: String -> Aff Response
html text = do
  body <- Class.liftEffect (stringToUint8Array text)
  pure
    { body
    , headers: [ Tuple.Tuple "Content-Type" "text/html" ]
    , status: StatusCode.status200
    }

stringToUint8Array :: String -> Effect Uint8Array
stringToUint8Array s = do
  b <- Buffer.fromString s Encoding.UTF8
  ab <- Buffer.toArrayBuffer b
  TypedArray.whole ab

app :: Request -> Aff Response
app { headers } = do
  -- TODO: request.remoteAddress
  let
    host =
      Maybe.maybe
        "unknown"
        Tuple.snd
        (Foldable.find ((eq "host") <<< Tuple.fst) headers)
  html host

main :: Effect Unit
main = do
  portMaybe <- map (bindFlipped Int.fromString) (Process.lookupEnv "PORT")
  let config = { hostname: "0.0.0.0", port: Maybe.fromMaybe 8080 portMaybe }
  Server.run config (Console.log "listen") app
