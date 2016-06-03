module FRP.Purelyst.Behavior where

import Prelude((>>>), const)
import Data.Identity(Identity)
import FRP.Purelyst.Reactive(Reactive(..), Step, stepper)

newtype Behavior t a = Behavior (Reactive Identity t a)

beh :: forall t a. (t -> a) -> Behavior t a
beh = stepper >>> Behavior

unbeh :: forall t a. Behavior t a -> (t -> Step Identity t a)
unbeh (Behavior (Reactive r)) = r

always :: forall t a. a -> Behavior t a
always a = beh (const a)
