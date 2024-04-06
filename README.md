# mutable-array

Mutable arrays similar to [`linear-base`] with efficient (but unsafe)
splits and joins. They are unsafe because they allow an array to be split
into multiple slices and join assumes (but doesn't verify) that the
slices being joined are backed by the same array.


[`linear-base`]: https://hackage.haskell.org/package/linear-base
