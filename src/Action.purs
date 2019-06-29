module Action
  ( Action(..)
  ) where

import Prelude

import Bouzuya.HTTP.Method (Method)
import Data.String as String

data Action
  = ContactCreate
  | ContactList
  | HealthCheck
  | MethodNotAllowed (Array Method)
  | NotFound

derive instance eqAction :: Eq Action
instance showAction :: Show Action where
  show = case _ of
    ContactCreate -> "ContactCreate"
    ContactList -> "ContactList"
    HealthCheck -> "HealthCheck"
    (MethodNotAllowed methods) ->
      "MethodNotAllowed " <> (String.joinWith ", " (map show methods))
    NotFound -> "NotFound "
