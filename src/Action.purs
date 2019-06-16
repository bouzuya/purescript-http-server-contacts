module Action
  ( execute
  ) where

import Prelude

import Bouzuya.HTTP.StatusCode as StatusCode
import Middleware.Logging as MiddlewareLogging
import Middleware.PathNormalize as MiddlewarePathNormalize
import Middleware.Routing as MiddlewareRouting
import ResponseHelper as ResponseHelper
import Type (AppStore, NewHandler, Middleware)

type R1 r = (store :: AppStore | r)
type R2 r = MiddlewarePathNormalize.R r

execute :: NewHandler (R1 ())
execute = middleware handler

handler :: forall r. NewHandler r
handler _ = ResponseHelper.fromStatus StatusCode.status500 []

middleware :: forall a. Middleware (R1 ()) a
middleware =
  MiddlewareRouting.middleware
  >>> MiddlewareLogging.middleware
  >>> MiddlewarePathNormalize.middleware
