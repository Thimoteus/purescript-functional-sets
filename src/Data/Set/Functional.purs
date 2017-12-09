-- | This module identifies sets with their characteristic functions.
module Data.Set.Functional where

import Prelude

import Data.Decidable (class Decidable)
import Data.Decide (class Decide)
import Data.Divide (class Divide)
import Data.Divisible (class Divisible)
import Data.Either (Either(..))
import Data.Equivalence (Equivalence(..), defaultEquivalence)
import Data.Foldable (class Foldable, foldr)
import Data.Functor.Contravariant (class Contravariant)
import Data.Newtype (class Newtype, un)
import Data.Predicate (Predicate(..))
import Data.Tuple (Tuple(..))

newtype Set a = Set (Predicate a)

derive instance newtypeSet :: Newtype (Set a) _

set :: forall a. (a -> Boolean) -> Set a
set = Set <<< Predicate

owns :: forall a. Set a -> a -> Boolean
owns (Set (Predicate a)) = a

infix 4 owns as ∋

characteristic :: forall a. Set a -> Predicate a
characteristic (Set x) = x

instance heytingAlgebraSet :: HeytingAlgebra (Set a) where
  tt = universe
  ff = empty
  conj = intersection
  disj = union
  not = complement
  implies x y = complement x `union` y

instance booleanAlgebraSet :: BooleanAlgebra (Set a)

instance notFunctorSet :: Fail "Sets cannot be Functors. To change the type of a given Set while preserving structure, use cmap" => Functor Set where
  map _ _ = empty

instance notEqSet :: Fail "Sets cannot be decided to be equal since they are potentially infinite in size" => Eq (Set a) where
  eq _ _ = true

instance notShowSet :: Fail "Sets cannot be shown in a way that copy+pasting can return the original Set" => Show (Set a) where
  show _ = "Set"

derive newtype instance contravariantSet :: Contravariant Set

derive newtype instance divideSet :: Divide Set

derive newtype instance divisibleSet :: Divisible Set

derive newtype instance decideSet :: Decide Set

derive newtype instance decidableSet :: Decidable Set

-- | Test whether a value is in a set. This is the equivalent of `member` in `Data.Set`.
elem :: forall a. a -> Set a -> Boolean
elem = flip owns

infix 4 elem as ∈

-- | The empty set.
empty :: forall a. Set a
empty = set (const false)

-- | The universal set relative to a type. Unfortunately we can encode a translation
-- | of Russell's pardox: ``universe `elem` universe = true``.`
universe :: forall a. Set a
universe = set (const true)

-- | For a given set x, the set of all values not in x.
complement :: forall a. Set a -> Set a
complement x = set (not <<< owns x)

-- | The set of all values in either of two sets.
union :: forall a. Set a -> Set a -> Set a
union x y = set \ a -> a ∈ x || a ∈ y

infixr 2 union as ∪

unions :: forall f a. Foldable f => f (Set a) -> Set a
unions = foldr union empty

-- | The disjoint union of two sets is a set that contains each set's values along
-- | with a tag of each value's provenance.
disjointUnion :: forall a b. Set a -> Set b -> Set (Either a b)
disjointUnion x y = set z where
  z (Left a) = a ∈ x
  z (Right a) = a ∈ y

infixr 2 disjointUnion as ⊔

-- | The set of all values in both of two sets.
intersection :: forall a. Set a -> Set a -> Set a
intersection x y = set \ a -> a ∈ x && a ∈ y

infixr 3 intersection as ∩

intersections :: forall a f. Foldable f => f (Set a) -> Set a
intersections = foldr intersection universe

-- | The cartesian product of two sets is a set where each value is a pair such
-- | that the first value is from the first set and the second is from the second
-- | set.
cartesianProduct :: forall a b. Set a -> Set b -> Set (Tuple a b)
cartesianProduct x y = set \ (Tuple a b) -> a ∈ x && b ∈ y

infixr 3 cartesianProduct as ×

-- | For two sets x and y, the set of values in x but not in y.
difference :: forall a. Set a -> Set a -> Set a
difference x y = set \ a -> a ∈ x && not (a ∈ y)

infixl 6 difference as ∖

-- | Use an equivalence relation to insert an element into a Set.
insertBy :: forall a. Equivalence a -> a -> Set a -> Set a
insertBy equals a x = set y where
  y b = if a `un Equivalence equals` b then true else b ∈ x

-- | Insert a single element into a Set.
insert :: forall a. Eq a => a -> Set a -> Set a
insert = insertBy defaultEquivalence

singletonBy :: forall a. Equivalence a -> a -> Set a
singletonBy equals a = set x where
  x b = a `un Equivalence equals` b

-- | Create a Set with a single member
singleton :: forall a. Eq a => a -> Set a
singleton = singletonBy defaultEquivalence

deleteBy :: forall a. Equivalence a -> a -> Set a -> Set a
deleteBy equals a x = set y where
  y b = if a `un Equivalence equals` b then false else b ∈ x

-- | Remove an element from a Set
delete :: forall a. Eq a => a -> Set a -> Set a
delete = deleteBy defaultEquivalence

fromFoldableBy :: forall f a. Foldable f => Equivalence a -> f a -> Set a
fromFoldableBy p = foldr (insertBy p) empty

-- | Turn a foldable container into a Set
fromFoldable :: forall f a. Eq a => Foldable f => f a -> Set a
fromFoldable = fromFoldableBy defaultEquivalence