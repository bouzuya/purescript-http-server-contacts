module Main
  ( main
  ) where

import Prelude

import App as App
import Bouzuya.HTTP.Server as Server
import Control.Bind (bindFlipped)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Traversable as Traversable
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Console as Console
import Node.Process as Process
import Store as Store
import Type (Contact)

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
  store <- Store.empty
  _ <-
    Traversable.sequence
      (map
        (\c -> Store.insert store c.name c)
        initialContacts)
  port <- Class.liftEffect (readPort 8080)
  let config = { host: "0.0.0.0", port }
  Class.liftEffect
    (Server.run
      config
      (\{ host, port: p } ->
        Console.log ("listening on http://" <> host <> ":" <> show p))
      (\request -> App.execute { request, store }))
