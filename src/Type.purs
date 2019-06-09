module Type
  ( Contact
  , AppStore
  ) where

import Store (Store)

type Contact =
  { name :: String
  , address :: String
  , tel :: String
  }

type AppStore = Store (Array Contact)
