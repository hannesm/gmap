(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
(* this code wouldn't exist without Justus Matthiesen, thanks for the help! *)

(** Heterogenous maps over a GADT.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** Ordering. *)
module Order : sig

  (** {1:order Ordering} *)

  (** The ordering type embedding type equality for [Eq]. *)
  type (_,_) t =
    | Lt : ('a, 'b) t
    | Eq : ('a, 'a) t
    | Gt : ('a, 'b) t
end

(** Key. *)
module type KEY = sig

  (** {1:key Key} *)

  type _ t
  (** The type of a key *)

  val compare : 'a t -> 'b t -> ('a, 'b) Order.t
  (** [compare k k'] is the total order of keys. *)

  val pp : Format.formatter -> 'a t -> 'a -> unit
  (** [pp k] is the pretty-printer. *)
end

(** Output signature of the functor {!Make} *)
module type S = sig

  (** {1:skey Key} *)

  type 'a key
  (** The type for keys whose lookup value is ['a]. *)

  (** {1:map Map} *)

  type t
  (** The type of the map *)

  (** {2 Constructors} *)

  val empty : t
  (** [empty] is the empty map. *)

  val singleton : 'a key -> 'a -> t
  (** [singleton key value] creates a map with the single provided binding. *)

  (** {2 Basic operations} *)

  val is_empty : t -> bool
  (** [is_empty t] is [true] if the map is empty, [false] otherwise. *)

  val cardinal : t -> int
  (** [cardinal t] is the number of bindings in [t]. *)

  (** {2 Lookup operations} *)

  val mem : 'a key -> t -> bool
  (** [mem key t] is [true] if [key] is bound in [t]. *)

  val find : 'a key -> t -> 'a option
  (** [find key t] is the value [Some value] of [key] in [t], [None] if [key]
      is not bound in [t]. *)

  val get : 'a key -> t -> 'a
  (** [find key t] is the value of [key] in [t].

      @raise Not_found if [key] is not bound in [t]. *)

  (** {2 Insertion and removal operations} *)

  val add : 'a key -> 'a -> t -> t
  (** [add key value t] adds the binding [V (key, value)] to [t]. *)

  val remove : 'a key -> t -> t
  (** [remove key t] removes the binding from [t]. *)

  (** {1 Bindings} *)

  type v = V : 'a key * 'a -> v
  (** The type for a binding: a key and its value. *)

  (** {2 Selection} *)

  val min_binding : t -> v option
  (** [min_binding t] is the minimal binding in [t], [None] if [t] is empty. *)

  val max_binding : t -> v option
  (** [max_binding t] is the maximal binding in [t], [None] if [t] is empty. *)

  val any_binding :  t -> v option
  (** [any_binding t] is any binding in [t], [None] if [t] is empty. *)

  val bindings : t -> v list
  (** [bindings t] is the list of bindings in [t]. *)

  (** {2 Lookup} *)

  val findv : 'a key -> t -> v option
  (** [findv key t] is the binding [Some v] of [key] in [t], [None] if [key]
      is not bound in [t]. *)

  val getv : 'a key -> t -> v
  (** [getv key t] is the binding of [key] in [t].

      @raise Not_found if [key] is not bound in [t]. *)


  (** {2 Insertion} *)

  val addv : v -> t -> t
  (** [addv v t] adds the binding [v] to [t]. *)

  (** {2 Equality} *)

  val equal : (v -> v -> bool) -> t -> t -> bool
  (** [equal p t t'] is [true] if all bindings of [t] and [t'] satisfy [p] *)

  (** {2 Higher-order functions} *)

  val iter : (v -> unit) -> t -> unit
  (** [iter f t] iterates over [t] and applies [f] to each binding. *)

  val fold : (v -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f t acc] folds over the bindings of [t] with [f], starting with [acc]. *)

  val for_all : (v -> bool) -> t -> bool
  (** [for_all p t] is [true] if all bindings in [t] satisfy [p], [false] otherwise. *)

  val exists : (v -> bool) -> t -> bool
  (** [exists p t] is [true] if some binding in [t] satisfies [p], [false] otherwise. *)

  val filter : (v -> bool) -> t -> t
  (** [filter p t] is the map of all bindings in [t] that satisfy [p]. *)

  val merge : (v option -> v option -> v option) -> t -> t -> t
  (** [merge f t t'] merges [t] and [t'] by using the result of [f] for every binding. *)

  val union : (v -> v -> v option) -> t -> t -> t
  (** [union f t t'] is the union of [t] and [t']. If the same binding is defined
      in both maps, the result of [f] is used. *)

  (** {2 Pretty printer} *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt t] is a pretty printer of the map [t]. *)
end

(** Functor for heterogenous maps whose keys are provided by [Key]. *)
module Make : functor (Key : KEY) -> S with type 'a key = 'a Key.t
