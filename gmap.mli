(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
(* this code wouldn't exist without Justus Matthiesen, thanks for the help! *)

(** Heterogenous maps over a GADT.

    The motivation for this library originated in the area of parsing binary
    network protocols, which often contain options and extensions in the form of
    tag, length, value encodings: the set of tags and corresponding values is
    specified in some Internet standard, and later extended by using a global
    registry.  Examples are IP options, TCP options, DNS resource records, TLS
    hello extensions, X.509v3 extensions, ... These extension mechanisms usually
    include the invariant that each tag may only be present once.

    A more naive approach is to use a variant type of all known tag-value
    combinations and storing these in an association list while parsing, but
    verifying the uniqueness invariant takes quadratic ([O(n^2)]) time, and
    retrieving a specific option is only doable in linear [O(n)] time.
    Additionally, variant packing/unpacking is required.

    In gmap, {{:https://en.wikipedia.org/wiki/Generalized_algebraic_data_type}GADTs}
    are used to provide key-dependent value types: each GADT constructor carries
    their value type.  The underlying storage mechanism uses OCaml's stdlib
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.html}Map} type:
    Lookup takes [O(log n)] time.  The above mentioned uniqueness invariant can
    be verified by only using {!S.update} or {!S.add_unless_bound} (respectively
    {!S.addb_unless_bound} for insertion).

    A simple example:

{[
type _ key =
  | A : int key
  | B : string key

module K = struct
  type 'a t = 'a key

  let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t = fun t t' ->
    let open Gmap.Order in
    match t, t' with
    | A, A -> Eq | A, _ -> Lt | _, A -> Gt
    | B, B -> Eq
end

module GM = Gmap.Make(K)
]}

    Using [GM] is done as follows:

{[
match GM.find A m with
| Some x -> x * x
| None -> 0
]}

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** Ordering. *)
module Order : sig

  (** The ordering type embedding type equality for [Eq]. *)
  type (_,_) t =
    | Lt : ('a, 'b) t
    | Eq : ('a, 'a) t
    | Gt : ('a, 'b) t
end

(** Key. *)
module type KEY = sig

  type _ t
  (** The type of a key *)

  val compare : 'a t -> 'b t -> ('a, 'b) Order.t
  (** [compare k k'] is the total order of keys. *)
end

(** Output signature of the functor {!Make} *)
module type S = sig

  type 'a key
  (** The type for map keys whose lookup value is ['a]. *)

  type t
  (** The type of maps from type ['a key] to ['a]. *)

  (** {2 Constructors} *)

  val empty : t
  (** [empty] is the empty map. *)

  val singleton : 'a key -> 'a -> t
  (** [singleton key value] creates a one-element map that contains a binding
      [value] for [key]. *)

  (** {2 Basic operations} *)

  val is_empty : t -> bool
  (** [is_empty m] returns [true] if the map [m] is empty, [false] otherwise. *)

  val cardinal : t -> int
  (** [cardinal m] returns the number of bindings of the map [m]. *)

  (** {2 Lookup operations} *)

  val mem : 'a key -> t -> bool
  (** [mem key m] returns [true] if the map [m] contains a binding for [key]. *)

  val find : 'a key -> t -> 'a option
  (** [find key m] returns [Some v] if the binding of [key] in [m] is [v], or
      [None] if [key] is not bound [m]. *)

  val get : 'a key -> t -> 'a
  (** [find key m] returns [v] if the binding of [key] in [m] is [v].

      @raise Not_found if [m] does not contain a binding for [key]. *)

  (** {2 Insertion and removal operations} *)

  val add_unless_bound : 'a key -> 'a -> t -> t option
  (** [add_unless_bound key value m] returns [Some m'], a map containing the
      same bindings as [m], plus a binding of [key] to [value].  Or, [None] if
      [key] was already bound in [m]. *)

  val add : 'a key -> 'a -> t -> t
  (** [add key value m] returns a map containing the same bindings as [m], plus
      a binding of [key] to [value].  If [key] was already bound in [m], the
      previous binding disappears. *)

  val remove : 'a key -> t -> t
  (** [remove key m] returns a map containing the same bindings as [m], except
      for [key] which is not bound in the returned map.  If [key] was not bound
      in [m], [m] is returned unchanged. *)

  val update : 'a key -> ('a option -> 'a option) -> t -> t
  (** [update k f m] returns a map containing the same bindings as [m], except
      for the binding [v] of [k].  Depending the value of [v], which is
      [f (find k m)], the binding of [k] is added, removed, or updated. *)


  (** {2 Keys and Bindings} *)

  type k = K : 'a key -> k
  (** The monomorphic type of a key. *)

  type b = B : 'a key * 'a -> b
  (** The type for a binding: a pair containing a key and its value. *)

  (** {2 Key operations} *)

  val comparek : k -> k -> int
  (** [compare k k'] compares [k] with [k'], using the ordering. *)

  val memk : k -> t -> bool
  (** [memk key m] returns [true] if the map [m] contains a binding for [key]. *)

  val getk : k -> t -> b
  (** [k key m] returns [Some v] if the binding of [key] in [m] is [v], or
      [None] if [key] is not bound [m]. *)

  val findk : k -> t -> b option
  (** [findk key m] returns [Some v] if the binding of [key] in [m] is [v], or
      [None] if [key] is not bound [m]. *)

  val removek : k -> t -> t
  (** [removek key m] returns a map containing the same bindings as [m], except
      for [key] which is not bound in the returned map.  If [key] was not bound
      in [m], [m] is returned unchanged. *)

  (** {2 Selection of bindings} *)

  val min_binding : t -> b option
  (** [min_binding m] is the minimal binding in [m], [None] if [m] is empty. *)

  val max_binding : t -> b option
  (** [max_binding m] is the maximal binding in [m], [None] if [m] is empty. *)

  val any_binding :  t -> b option
  (** [any_binding m] is any binding in [m], [None] if [m] is empty. *)

  val bindings : t -> b list
  (** [bindings m] returns the list of all bindings in the given map [m].  The
      list is sorted with respect to the ordering over the type of the keys. *)

  (** {2 Lookup of bindings} *)

  val findb : 'a key -> t -> b option
  (** [findb key m] returns [Some b] if the binding of [key] in [m] is [b], or
      [None] if [key] is not bound in [t]. *)

  val getb : 'a key -> t -> b
  (** [getb key m] returns [v] if the binding of [key] in [m] is [b].

      @raise Not_found if [m] does not contain a binding for [key]. *)


  (** {2 Insertion of bindings} *)

  val addb_unless_bound : b -> t -> t option
  (** [addb_unless_bound b m] returns [Some m'], a map containing the
      same bindings as [m], plus the binding [b].  Or, [None] if
      [key], the first part of [b], was already bound in [m]. *)

  val addb : b -> t -> t
  (** [addb b m] returns a map containing the same bindings as [m], plus the
      binding [b].  If [key], the first part of [b] was already bound in [m],
      the previous binding disappears. *)

  (** {2 Equality} *)

  val equal : (b -> b -> bool) -> t -> t -> bool
  (** [equal p m m'] tests whether the maps [m] and [m'] are equal, that is
      contain equal keys and associate them with equal data.  [p] is the
      equality predicate used to compare the data associated with the keys. *)

  (** {2 Higher-order functions} *)

  val iter : (b -> unit) -> t -> unit
  (** [iter f m] applies [f] to all bindings in [m].  The bindings are passed in
      increasing order with respect to the ordering over the type of keys. *)

  val fold : (b -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f m acc] computes [(f bN .. (f b1 acc))], where [b1 .. bN] are the
      bindings of [m] in increasing order over the type of keys. *)

  val for_all : (b -> bool) -> t -> bool
  (** [for_all p m] checks if all bindings of the map [m] satisfy the predicate
      [p]. *)

  val exists : (b -> bool) -> t -> bool
  (** [exists p m] checks if at least one binding of the map [m] satisfies
      [p]. *)

  val filter : (b -> bool) -> t -> t
  (** [filter p m] returns the map with all the bindings in [m] that satisfy
      [p]. *)

  val merge : (b option -> b option -> b option) -> t -> t -> t
  (** [merge f m m'] computes a map whose keys is a subset of keys of [m] and
      [m'].  The presence of each such binding, and the corresponding value, is
      determined with the function [f]. *)

  val union : (b -> b -> b option) -> t -> t -> t
  (** [union f m m'] computes a map whose keys is the union of the keys of [m]
      and [m'].  When the same binding is defined in both maps, the function [f]
      is used to combine them. *)
end

(** Functor for heterogenous maps whose keys are provided by [Key]. *)
module Make : functor (Key : KEY) -> S with type 'a key = 'a Key.t
