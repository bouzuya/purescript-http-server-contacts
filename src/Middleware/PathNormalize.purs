module Middleware.PathNormalize
  ( R
  , middleware
  ) where

import Prelude

import Data.Symbol (SProxy)
import Data.Symbol as Symbol
import NormalizedPath (NormalizedPath)
import NormalizedPath as NormalizedPath
import Prim.Row (class Lacks)
import Record as Record
import ResponseHelper as ResponseHelper
import Type (Middleware)

type R r = (normalizedPath :: NormalizedPath | r)

middleware :: forall r. Lacks "normalizedPath" r => Middleware r (R r)
middleware next context@{ request: { pathname } } = do
  let
    normalized = NormalizedPath.normalize pathname
    normalizedPathname = NormalizedPath.toString normalized
  if pathname /= normalizedPathname
    then ResponseHelper.status301 normalizedPathname
    else next (Record.insert symbol normalized context)

symbol :: SProxy "normalizedPath"
symbol = Symbol.SProxy
