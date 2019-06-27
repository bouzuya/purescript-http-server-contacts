module Middleware.Routing
  ( R
  , middleware
  ) where

import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.Request.NormalizedPath (NormalizedPath)
import Data.Symbol (SProxy)
import Data.Symbol as Symbol
import Middleware.PathNormalize as MiddlewarePathNormalize
import Prim.Row (class Lacks)
import Record as Record
import Type (Middleware)

type R a r = MiddlewarePathNormalize.R (action :: a | r)

middleware ::
  forall a r.
  Lacks "action" r
  => Lacks "normalizedPath" r
  => (NormalizedPath -> Method -> a)
  -> Middleware (MiddlewarePathNormalize.R r) (R a r)
middleware route next context@{ normalizedPath, request: { method } } = do
  let action = route normalizedPath method
  next (Record.insert symbol action context)

symbol :: SProxy "action"
symbol = Symbol.SProxy
