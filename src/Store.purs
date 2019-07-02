module Store
  ( Store
  , empty
  , get
  , insert
  , list
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Class as Class
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Id = String
newtype Store a = Store (Ref (Array (Tuple Id a)))

empty :: forall a. Aff (Store a)
empty = Class.liftEffect (map Store (Ref.new []))

get :: forall a. Store a -> Id -> Aff (Maybe a)
get (Store ref) id = do
  xs <- Class.liftEffect (Ref.read ref)
  pure (map Tuple.snd (Array.find ((eq id) <<< Tuple.fst) xs))

insert :: forall a. Store a -> Id -> a -> Aff Unit
insert (Store ref) id x = do
  xs <- Class.liftEffect (Ref.read ref)
  Class.liftEffect (Ref.write (Array.snoc xs (Tuple.Tuple id x)) ref)

list :: forall a. Store a -> Aff (Array a)
list (Store ref) = Class.liftEffect (map (map Tuple.snd) (Ref.read ref))
