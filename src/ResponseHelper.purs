module ResponseHelper
  ( html
  , html'
  , json
  , json'
  ) where

import Prelude

import Bouzuya.HTTP.Response (Response)
import Bouzuya.HTTP.StatusCode (StatusCode)
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.ArrayBuffer.Typed as TypedArray
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as Class
import Node.Buffer as Buffer
import Node.Encoding as Encoding

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

stringToUint8Array :: String -> Effect Uint8Array
stringToUint8Array s = do
  b <- Buffer.fromString s Encoding.UTF8
  ab <- Buffer.toArrayBuffer b
  TypedArray.whole ab
