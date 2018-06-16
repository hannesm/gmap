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
  val add : 'a key -> 'a -> t -> t
  val remove : 'a key -> t -> t

  type v = V : 'a key * 'a -> v
  val min_binding : t -> v option
  val max_binding : t -> v option
  val any_binding :  t -> v option
  val bindings : t -> v list

  val findv : 'a key -> t -> v option
  val getv : 'a key -> t -> v

  val addv : v -> t -> t

  val equal : (v -> v -> bool) -> t -> t -> bool

  val iter : (v -> unit) -> t -> unit
  val fold : (v -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (v -> bool) -> t -> bool
  val exists : (v -> bool) -> t -> bool
  val filter : (v -> bool) -> t -> t
  val merge : (v option -> v option -> v option) -> t -> t -> t
  val union : (v -> v -> v option) -> t -> t -> t
  val pp : Format.formatter -> t -> unit
end

module Make (Key : KEY) : S with type 'a key = 'a Key.t = struct
  type 'a key = 'a Key.t
  type k = K : 'a key -> k
  type v = V : 'a key * 'a -> v

  module M = Map.Make(struct
      type t = k
      let compare (K a) (K b) = match Key.compare a b with
        | Order.Lt -> -1
        | Order.Eq -> 0
        | Order.Gt -> 1
    end)

  type t = v M.t

  let empty = M.empty
  let singleton k v = M.singleton (K k) (V (k, v))

  let is_empty = M.is_empty
  let mem k m = M.mem (K k) m

  let addv (V (k, _) as v) m = M.add (K k) v m
  let add k v m = M.add (K k) (V (k, v)) m

  let remove k m = M.remove (K k) m

  let getv : type a. a Key.t -> t -> v = fun k t ->
    match M.find (K k) t with
    | V (k', v) ->
      match Key.compare k k' with
      | Order.Eq -> V (k, v)
      | _ -> assert false
  let get : type a. a Key.t -> t -> a = fun k t ->
    match M.find (K k) t with
    | V (k', v) ->
      match Key.compare k k' with
      | Order.Eq -> v
      | _ -> assert false

  let findv : type a. a Key.t -> t -> v option = fun k t ->
    try Some (getv k t) with Not_found -> None
  let find : type a. a Key.t -> t -> a option = fun k t ->
    try Some (get k t) with Not_found -> None

  let any_binding t = try Some (snd (M.choose t)) with Not_found -> None
  let min_binding t = try Some (snd (M.min_binding t)) with Not_found -> None
  let max_binding t = try Some (snd (M.max_binding t)) with Not_found -> None
  let bindings t = snd (List.split (M.bindings t))

  let cardinal t = M.cardinal t

  let for_all f t = M.for_all (fun _ b -> f b) t
  let exists f t = M.exists (fun _ b -> f b) t

  let iter f t = M.iter (fun _ b -> f b) t
  let fold f t acc = M.fold (fun _ b acc -> f b acc) t acc
  let filter f t = M.filter (fun _ b -> f b) t

  let merge f a b =
    M.merge (fun _ v1 v2 -> f v1 v2) a b

  let union f a b =
    M.union (fun _ v1 v2 -> f v1 v2) a b

  let pp ppf t =
    let pp ppf = function
      | V (k, v) -> Key.pp ppf k v
    in
    Fmt.(list ~sep:(unit "@.") pp) ppf (bindings t)

  let equal cmp a b = M.equal cmp a b
end
