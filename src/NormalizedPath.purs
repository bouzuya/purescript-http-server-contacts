module NormalizedPath
  ( NormalizedPath
  , normalize
  , toPieces
  , toString
  ) where

import Prelude

import Data.Array as Array
import Data.String as String

newtype NormalizedPath = NormalizedPath (Array String)

normalize :: String -> NormalizedPath
normalize =
  NormalizedPath
    <<< (Array.filter (not <<< String.null))
    <<< (String.split (String.Pattern "/"))

toPieces :: NormalizedPath -> Array String
toPieces (NormalizedPath parsed) = parsed

toString :: NormalizedPath -> String
toString (NormalizedPath parsed) = "/" <> (String.joinWith "/" parsed)
