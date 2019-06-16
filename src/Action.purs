module Action
  ( execute
  ) where

import Bouzuya.HTTP.StatusCode as StatusCode
import Middleware.Logging as MiddlewareLogging
import Middleware.PathNormalize as MiddlewarePathNormalize
import Middleware.Routing as MiddlewareRouting
import ResponseHelper as ResponseHelper
import Type (AppStore, NewHandler)

type R1 r = (store :: AppStore | r)
type R2 r = MiddlewarePathNormalize.R r

execute :: NewHandler (R1 ())
execute =
  MiddlewarePathNormalize.middleware
    (MiddlewareLogging.middleware
      (MiddlewareRouting.middleware
        handler))

handler :: forall r. NewHandler r
handler _ = ResponseHelper.fromStatus StatusCode.status500 []
