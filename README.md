# purescript-functional-sets

Mathematically, a set `X` can be identified with its characteristic function `χ`:

```
x ∈ X ⇔ χ(x) = 1
```

This package defines a type `Set a` as a newtype over `a -> Boolean`.

## Tradeoffs

Disadvantages:

1. None of the instances in `Data.Set` are represented here.
2. There are no `size`, `subset`, `properSubset`, `isEmpty`, `toUnfoldable`, `map` functions.

Advantages:

1. There is a law-abiding `Contravariant` instance, as well as instances for `Divide`, `Decide` (and friends), and `BooleanAlgebra`.
2. In addition to the `empty` set, there is the `universe`-al set, which is the set of all values of a given type. Thus we can also define the `complement` of any set.
3. There are two `newtype`s corresponding to the monoids of a powerset under union and intersection.