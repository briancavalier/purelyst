module FRP.Purelyst.Behavior where

import Prelude((>>>), const, map, class Functor, class Show)
import Data.Identity(Identity)
import FRP.Purelyst.Reactive(Reactive, stepper) as R

newtype Behavior t a = Behavior (R.Reactive Identity t a)

beh :: forall t a. (t -> a) -> Behavior t a
beh = R.stepper >>> Behavior

always :: forall t a. a -> Behavior t a
always a = beh (const a)

instance showBehavior :: Show (Behavior t a) where
  show _ = "Behavior" -- lame

instance functorBehavior :: Functor (Behavior t) where
  map f (Behavior r) = Behavior (map f r)
