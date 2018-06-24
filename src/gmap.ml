(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)

(* this code wouldn't exist without Justus Matthiesen, thanks for the help! *)

module Order = struct
  type (_,_) t =
    | Lt : ('a, 'b) t
    | Eq : ('a, 'a) t
    | Gt : ('a, 'b) t
end

module type KEY = sig
  type _ t
  val compare : 'a t -> 'b t -> ('a, 'b) Order.t
  val pp : Format.formatter -> 'a t -> 'a -> unit
end

module type S = sig
  type 'a key
  type t

  val empty : t
  val singleton : 'a key -> 'a -> t
  val is_empty : t -> bool
  val cardinal : t -> int
  val mem : 'a key -> t -> bool
  val find : 'a key -> t -> 'a option
  val get : 'a key -> t -> 'a
  val add_unless_bound : 'a key -> 'a -> t -> t option
  val add : 'a key -> 'a -> t -> t
  val remove : 'a key -> t -> t
  val update : 'a key -> ('a option -> 'a option) -> t -> t

  type b = B : 'a key * 'a -> b
  val min_binding : t -> b option
  val max_binding : t -> b option
  val any_binding :  t -> b option
  val bindings : t -> b list

  val findb : 'a key -> t -> b option
  val getb : 'a key -> t -> b

  val addb_unless_bound : b -> t -> t option
  val addb : b -> t -> t

  val equal : (b -> b -> bool) -> t -> t -> bool

  val iter : (b -> unit) -> t -> unit
  val fold : (b -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (b -> bool) -> t -> bool
  val exists : (b -> bool) -> t -> bool
  val filter : (b -> bool) -> t -> t
  val merge : (b option -> b option -> b option) -> t -> t -> t
  val union : (b -> b -> b option) -> t -> t -> t
  val pp : Format.formatter -> t -> unit
end

module Make (Key : KEY) : S with type 'a key = 'a Key.t = struct
  type 'a key = 'a Key.t
  type k = K : 'a key -> k
  type b = B : 'a key * 'a -> b

  module M = Map.Make(struct
      type t = k
      let compare (K a) (K b) = match Key.compare a b with
        | Order.Lt -> -1
        | Order.Eq -> 0
        | Order.Gt -> 1
    end)

  type t = b M.t

  let empty = M.empty
  let singleton k v = M.singleton (K k) (B (k, v))

  let is_empty = M.is_empty
  let mem k m = M.mem (K k) m

  let add k v m = M.add (K k) (B (k, v)) m
  let addb (B (k, v)) m = add k v m

  let add_unless_bound k v m = if mem k m then None else Some (add k v m)

  let addb_unless_bound (B (k, v)) m = add_unless_bound k v m

  let remove k m = M.remove (K k) m

  let getb : type a. a key -> t -> b = fun k m ->
    match M.find (K k) m with
    | B (k', v) ->
      match Key.compare k k' with
      | Order.Eq -> B (k, v)
      | _ -> assert false

  let get : type a. a key -> t -> a = fun k m ->
    match M.find (K k) m with
    | B (k', v) ->
      match Key.compare k k' with
      | Order.Eq -> v
      | _ -> assert false

  let findb : type a. a key -> t -> b option = fun k m ->
    try Some (getb k m) with Not_found -> None

  let find : type a. a key -> t -> a option = fun k m ->
    try Some (get k m) with Not_found -> None

  let update k f m =
    match f (find k m) with
    | None -> remove k m
    | Some v -> add k v m


  let any_binding m = try Some (snd (M.choose m)) with Not_found -> None
  let min_binding m = try Some (snd (M.min_binding m)) with Not_found -> None
  let max_binding m = try Some (snd (M.max_binding m)) with Not_found -> None
  let bindings m = snd (List.split (M.bindings m))

  let cardinal m = M.cardinal m

  let for_all p m = M.for_all (fun _ b -> p b) m
  let exists p m = M.exists (fun _ b -> p b) m

  let iter f m = M.iter (fun _ b -> f b) m
  let fold f m acc = M.fold (fun _ b acc -> f b acc) m acc
  let filter p m = M.filter (fun _ b -> p b) m

  let merge f m m' = M.merge (fun _ b b' -> f b b') m m'

  let union f m m' = M.union (fun _ b b' -> f b b') m m'

  let pp ppf m =
    let pp ppf = function
      | B (k, v) -> Key.pp ppf k v
    in
    Fmt.(list ~sep:(unit "@.") pp) ppf (bindings m)

  let equal cmp m m' = M.equal cmp m m'
end
