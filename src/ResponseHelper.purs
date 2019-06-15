module ResponseHelper
  ( html
  , html'
  , json
  , json'
  , sendStatus
  ) where

import Prelude

import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (StatusCode)
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as Class
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Simple.JSON as SimpleJSON

json :: String -> Aff Response
json = json' StatusCode.status200

json' :: StatusCode -> String -> Aff Response
json' status text = do
  body <- Class.liftEffect (stringToUint8Array text)
  pure
    { body
    , headers: [ Tuple.Tuple "Content-Type" "application/json" ]
    , status
    }

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

sendStatus :: StatusCode -> Array (Tuple String String) -> Aff Response
sendStatus status headers = do
  let text = SimpleJSON.writeJSON { message: show status }
  body <- Class.liftEffect (stringToUint8Array text)
  pure
    { body
    , headers: headers <> [ Tuple.Tuple "Content-Type" "application/json" ]
    , status
    }

-- private

stringToUint8Array :: String -> Effect Uint8Array
stringToUint8Array s = do
  b <- Buffer.fromString s Encoding.UTF8
  ab <- Buffer.toArrayBuffer b
  TypedArray.whole ab
