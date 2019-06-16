module Type
  ( Contact
  , AppStore
  , Middleware
  , NewHandler
  ) where

import Bouzuya.HTTP.Request (Request)
import Bouzuya.HTTP.Response (Response)
import Effect.Aff (Aff)
import Store (Store)

type Middleware a b = NewHandler b -> NewHandler a
type NewHandler r = Record (request :: Request | r) -> Aff Response

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type AppStore = Store (Array Contact)
