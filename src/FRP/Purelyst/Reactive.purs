module FRP.Purelyst.Reactive where

import Prelude (class Show, class Monad, pure, map, class Functor, (>>>))
import Prelude (id) as P
import Data.Bifunctor (bimap)
import Data.Tuple (Tuple(..))

data Reactive m a b = Reactive (a -> Step m a b)
type Step m a b = m (Tuple b (Reactive m a b))

stepper :: forall m a b. Monad m => (a -> b) -> Reactive m a b
stepper f = Reactive (\a -> pure (Tuple (f a) (stepper f)))

-- TODO: Add Semigroupoid, Category instances
id :: forall m a. Monad m => Reactive m a a
id = stepper P.id

stepR :: forall m a b. Monad m => Reactive m a b -> a -> Step m a b
stepR (Reactive r) a = r a

instance showReactive :: Monad m => Show (Reactive m a b) where
  show _ = "Reactive" -- lame

instance functorReactive :: Monad m => Functor (Reactive m a) where
  map f (Reactive r) = Reactive (r >>> map (bimap f (map f)))
