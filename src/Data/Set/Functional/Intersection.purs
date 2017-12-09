-- | Monoid under intersection
module Data.Set.Functional.Intersection where

import Prelude
import Data.Monoid (class Monoid)
import Data.Set.Functional (Set, (∩), universe)

newtype SetIntersection a = Intersect (Set a)

unIntersect :: forall a. SetIntersection a -> Set a
unIntersect (Intersect a) = a

instance semigroupSetIntersect :: Semigroup (SetIntersection a) where
  append (Intersect x) (Intersect y) = Intersect (x ∩ y)

instance monoidSetIntersect :: Monoid (SetIntersection n) where
  mempty = Intersect universe