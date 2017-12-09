-- | Monoid under union
module Data.Set.Functional.Union where

import Prelude
import Data.Monoid (class Monoid)
import Data.Set.Functional (Set, (∪), empty)

newtype SetUnion a = Union (Set a)

unUnion :: forall a. SetUnion a -> Set a
unUnion (Union a) = a

instance semigroupSetUnion :: Semigroup (SetUnion a) where
  append (Union x) (Union y) = Union (x ∪ y)

instance monoidSetUnion :: Monoid (SetUnion n) where
  mempty = Union empty