module Middleware.Logging
  ( middleware
  ) where

import Prelude

import Effect.Class.Console as Console
import Type (Middleware)

middleware :: forall r. Middleware r r
middleware next context@{ request: { method, pathname } } = do
  Console.log ((show method) <> " " <> pathname)
  response <- next context
  pure response
