## 0.3.0 (2019-04-20)

* breaking: remove S.pp and K.pp, they weren't necessary and imposed a Fmt dependency
* breaking: remove S.findb, S.getb, S.addb, S.addb_unless_bound -- use S.find, S.get, S.add, S.add_unless_bound instead
* S.equal, S.map, S.merge, and S.union use semi-explicit polymorphism with a record type
* breaking: S.equal signature is now : { f : 'a key -> 'a -> 'a -> bool } -> t -> t -> bool
* new function S.map : { f : 'a key -> 'a -> 'a } -> t -> t
* breaking: S.merge signature is now : { f : 'a key -> 'a option -> 'a option -> 'a option } -> t -> t -> t
* breaking: S.union signature is now : { f : 'a key -> 'a -> 'a -> 'a option } -> t -> t -> t
* added some tests

## 0.2.1 (2019-02-16)

* move build system to dune

## 0.2.0 (2018-06-24)

* New function `update`.
* New function `add_unless_bound` and `addb_unless_bound`.
* Replace `type v = V : 'a key * 'a -> v` by `type b = B : 'a key * 'a -> b`.
* Renamed functions ending with `v` to `b`

## 0.1.0 (2018-06-16)

* Initial release
