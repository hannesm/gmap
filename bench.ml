(* the primary motivation for gmap was to avoid a list of variants
   (with side-conditions: each tag occurs only once, not ordered)

   an incomplete matrix of options:

            | first variant wins | last variant wins | error on multiple
------------+--------------------+-------------------+------------------
list        | VF                 | VL                | VE
map (via B) | MBF                | MBL               | MBE
direct map  | MDF                | MDL               | MDE
list -> map | VMF                | VML               | VME

evaluate space (rly?) + time of different implementations
- input a random (mostly valid) string
- parse it
- access fields (one, three, all)

- List.mem (polymorphic compare) vs List.exists (in error on multiple case)?
- different Gmap implemementations (vendored Map, wrapped Map, Obj.magic)
- different compare functions (big match, Obj.magic, polymorphic compare)
*)

open Rresult.R.Infix

module BE = EndianString.BigEndian
module BBE = EndianBytes.BigEndian

let guard p err = if p then Ok () else Error err

let parse_tlv data off =
  guard (String.length data - off >= 2) (`Msg "too small") >>= fun () ->
  let t = BE.get_uint8 data off
  and l = BE.get_uint8 data (succ off)
  in
  guard (String.length data - off >= l + 2) (`Msg "too small for value") >>| fun () ->
  let v = String.sub data (off + 2) l in
  ((t, l, v), off + l + 2)

let pint l s = match l with
  | 1 -> Ok (BE.get_uint8 s 0)
  | 2 -> Ok (BE.get_uint16 s 0)
  | 4 -> Ok (Int32.to_int (BE.get_int32 s 0)) (* only safe on 64 bit *)
  | _ -> Error (`Msg "unknown integer length")

(* here's the variant implementation *)
type variant =
  | A of int
  | B of string
  | C of int
  | D of int
  | E
  | F of int
  | G of string
  | Unknown of int * string

let eint v =
  let bytes =
    if v land 0xFF = v then begin
      let buf = Bytes.create 1 in
      BBE.set_int8 buf 0 v;
      buf
    end else if v land 0xFFFF = v then begin
      let buf = Bytes.create 2 in
      BBE.set_int16 buf 0 v;
      buf
    end else
      let buf = Bytes.create 4 in
      BBE.set_int32 buf 0 (Int32.of_int v);
      buf
  in
  Bytes.unsafe_to_string bytes

let encode vs =
  let one v =
    let tag, value = match v with
      | A n -> 0, eint n
      | B s -> 1, s
      | C n -> 2, eint n
      | D n -> 3, eint n
      | E -> 4, ""
      | F n -> 5, eint n
      | G s -> 6, s
      | Unknown (i, s) -> i, s
    in
    let hdr = Bytes.create 2 in
    BBE.set_int8 hdr 0 tag;
    BBE.set_int8 hdr 1 (String.length value);
    Bytes.unsafe_to_string hdr ^ value
  in
  String.concat "" (List.map one vs)

let parse_v_variant (t, l, v) =
  match t with
  | 0 -> pint l v >>| fun i -> A i
  | 1 -> Ok (B v)
  | 2 -> pint l v >>| fun i -> C i
  | 3 -> pint l v >>| fun i -> D i
  | 4 -> guard (l = 0) (`Msg "expected empty value") >>| fun () -> E
  | 5 -> pint l v >>| fun i -> F i
  | 6 -> Ok (G v)
  | n -> Ok (Unknown (n, v))

let[@inline always] icomp (a : int) (b : int) = compare a b

module VF = struct
  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok (List.rev acc)
      else
        parse_tlv data off >>= fun (tlv, off') ->
        match parse_v_variant tlv with
        | Ok variant -> go off' (variant :: acc)
        | Error (`Msg s) -> Error (`Msg s)
    in
    go 0 []
end

module VL = struct
  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        match parse_v_variant tlv with
        | Ok variant -> go off' (variant :: acc)
        | Error (`Msg s) -> Error (`Msg s)
    in
    go 0 []
end

module VE = struct
  let eq_variant a b = match a, b with
    | A _, A _
    | B _, B _
    | C _, C _
    | D _, D _
    | E, E
    | F _, F _
    | G _, G _ -> true
    | Unknown (a, _), Unknown (b, _) -> icomp a b = 0
    | _ -> false

  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        match parse_v_variant tlv with
        | Ok variant ->
          if List.exists (eq_variant variant) acc then
            Error (`Msg "duplicate tag")
          else
            go off' (variant :: acc)
        | Error (`Msg s) -> Error (`Msg s)
    in
    go 0 []
end

let find_v ?(tags = []) vs =
  let a =
    match List.find_opt (function A _ -> true | _ -> false) vs with
    | Some (A i) -> Some i
    | _ -> None
  and b =
    match List.find_opt (function B _ -> true | _ -> false) vs with
    | Some (B s) -> Some s
    | _ -> None
  and c =
    match List.find_opt (function C _ -> true | _ -> false) vs with
    | Some (C i) -> Some i
    | _ -> None
  and d =
    match List.find_opt (function D _ -> true | _ -> false) vs with
    | Some (D i) -> Some i
    | _ -> None
  and e =
    match List.find_opt (function E -> true | _ -> false) vs with
    | Some E -> Some ()
    | _ -> None
  and f =
    match List.find_opt (function F _ -> true | _ -> false) vs with
    | Some (F s) -> Some s
    | _ -> None
  in
  let tag_values = List.map (fun i ->
      match List.find_opt (function Unknown (i', _) -> i = i' | _ -> false) vs with
      | Some (Unknown (_, v)) -> Some v
      | _ -> None) tags
  in
  (a, b, c, d, e, f, tag_values)

(* here's the GADT / Gmap implementation *)
type _ k =
  | GA : int k
  | GB : string k
  | GC : int k
  | GD : int k
  | GE : unit k
  | GF : int k
  | GG : string k
  | GUnknown : int -> string k

let c_k : type a b . a k -> b k -> (a, b) Gmap.Order.t = fun a b ->
  let open Gmap.Order in
  match a, b with
  | GA, GA -> Eq | GA, _ -> Lt | _, GA -> Gt
  | GB, GB -> Eq | GB, _ -> Lt | _, GB -> Gt
  | GC, GC -> Eq | GC, _ -> Lt | _, GC -> Gt
  | GD, GD -> Eq | GD, _ -> Lt | _, GD -> Gt
  | GE, GE -> Eq | GE, _ -> Lt | _, GE -> Gt
  | GF, GF -> Eq | GF, _ -> Lt | _, GF -> Gt
  | GG, GG -> Eq | GG, _ -> Lt | _, GG -> Gt
  | GUnknown a, GUnknown b ->
    let r = icomp a b in
    if r = 0 then Eq else if r < 0 then Lt else Gt

module G = Gmap.Make(struct type 'a t = 'a k let compare = c_k end)

let find_g ?(tags = []) m =
  let a = G.find GA m
  and b = G.find GB m
  and c = G.find GC m
  and d = G.find GD m
  and e = G.find GE m
  and f = G.find GF m
  in
  let tag_values = List.map (fun i -> G.find (GUnknown i) m) tags in
  (a, b, c, d, e, f, tag_values)

let parse_v_gmap (t, l, v) =
  match t with
  | 0 -> pint l v >>| fun i -> G.B (GA, i)
  | 1 -> Ok (G.B (GB, v))
  | 2 -> pint l v >>| fun i -> G.B (GC, i)
  | 3 -> pint l v >>| fun i -> G.B (GD, i)
  | 4 -> guard (l = 0) (`Msg "expected empty value") >>| fun () -> G.B (GE, ())
  | 5 -> pint l v >>| fun i -> G.B (GF, i)
  | 6 -> Ok (G.B (GG, v))
  | n -> Ok (G.B (GUnknown n, v))

module MBF = struct
  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        match parse_v_gmap tlv with
        | Ok (G.B (k, v)) ->
          begin match G.add_unless_bound k v acc with
            | None -> go off' acc
            | Some acc' -> go off' acc'
          end
        | Error (`Msg s) -> Error (`Msg s)
    in
    go 0 G.empty
end

module MBL = struct
  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        match parse_v_gmap tlv with
        | Ok (G.B (k, v)) ->
          let acc' = G.add k v acc in
          go off' acc'
        | Error (`Msg s) -> Error (`Msg s)
    in
    go 0 G.empty
end

module MBE = struct
  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        match parse_v_gmap tlv with
        | Ok (G.B (k, v)) ->
          begin match G.add_unless_bound k v acc with
            | None -> Error (`Msg "duplicate tag")
            | Some acc' -> go off' acc'
          end
        | Error (`Msg s) -> Error (`Msg s)
    in
    go 0 G.empty
end

module MDF = struct
  let parse_v_gmap' (t, l, v) acc =
    let add t v = match G.add_unless_bound t v acc with
      | None -> acc
      | Some acc' -> acc'
    in
    match t with
    | 0 -> pint l v >>| fun i -> add GA i
    | 1 -> Ok (add GB v)
    | 2 -> pint l v >>| fun i -> add GC i
    | 3 -> guard (l = 0) (`Msg "expected empty value") >>| fun () -> add GE ()
    | 4 -> pint l v >>| fun i -> add GF i
    | 5 -> Ok (add GG v)
    | n -> Ok (add (GUnknown n) v)

  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        parse_v_gmap' tlv acc >>= fun acc' ->
        go off' acc'
    in
    go 0 G.empty
end

module MDL = struct
  let parse_v_gmap' (t, l, v) acc =
    match t with
    | 0 -> pint l v >>| fun i -> G.add GA i acc
    | 1 -> Ok (G.add GB v acc)
    | 2 -> pint l v >>| fun i -> G.add GC i acc
    | 3 -> guard (l = 0) (`Msg "expected empty value") >>| fun () -> G.add GE () acc
    | 4 -> pint l v >>| fun i -> G.add GF i acc
    | 5 -> Ok (G.add GG v acc)
    | n -> Ok (G.add (GUnknown n) v acc)

  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        parse_v_gmap' tlv acc >>= fun acc' ->
        go off' acc'
    in
    go 0 G.empty
end

module MDE = struct
  let parse_v_gmap' (t, l, v) acc =
    let add t v = match G.add_unless_bound t v acc with
      | None -> Error (`Msg "duplicate tag")
      | Some acc' -> Ok acc'
    in
    match t with
    | 0 -> pint l v >>= fun i -> add GA i
    | 1 -> add GB v
    | 2 -> pint l v >>= fun i -> add GC i
    | 3 -> guard (l = 0) (`Msg "expected empty value") >>= fun () -> add GE ()
    | 4 -> pint l v >>= fun i -> add GF i
    | 5 -> add GG v
    | n -> add (GUnknown n) v

  let parse data =
    let l = String.length data in
    let rec go off acc =
      if off = l then
        Ok acc
      else
        parse_tlv data off >>= fun (tlv, off') ->
        parse_v_gmap' tlv acc >>= fun acc' ->
        go off' acc'
    in
    go 0 G.empty
end

let of_variant = function
  | A i -> G.B (GA, i)
  | B s -> G.B (GB, s)
  | C i -> G.B (GC, i)
  | D i -> G.B (GD, i)
  | E -> G.B (GE, ())
  | F i -> G.B (GF, i)
  | G s -> G.B (GG, s)
  | Unknown (t, v) -> G.B (GUnknown t, v)

module VMF = struct
  let parse data =
    VL.parse data >>| fun acc ->
    List.fold_left (fun m v ->
        let (G.B (k, v)) = of_variant v in
        G.add k v m)
      G.empty acc
end

module VML = struct
  let parse data =
    VL.parse data >>| fun acc ->
    List.fold_left (fun m v ->
        let (G.B (k, v)) = of_variant v in
        match G.add_unless_bound k v m with
        | None -> m
        | Some m' -> m')
      G.empty acc
end

module VME = struct
  let parse data =
    VL.parse data >>= fun acc ->
    List.fold_left (fun m v ->
        m >>= fun m ->
        let (G.B (k, v)) = of_variant v in
        match G.add_unless_bound k v m with
        | None -> Error (`Msg "duplicate tag")
        | Some m' -> Ok m')
      (Ok G.empty) acc
end

let iterations = 1_000_000

let vs =
  let base = [ A 5 ; D 10 ; E ] in
  let rec more acc = function
    | 0 -> acc
    | n ->
      let tag = 6 + Random.int 250
      and l = Random.int 250
      in
      (* this is to ensure uniqueness of tags -- otherwise the E modules above error out early *)
      let rec tag' c =
        if List.exists (fun (t, _) -> t = c) acc then tag' (succ c mod 256) else c
      in
      more ((tag' tag, Bytes.(unsafe_to_string (create l))) :: acc) (pred n)
  in
  base @ List.map (fun (t, v) -> Unknown (t, v)) (more [] 100)

let () =
  (* take a ts, run *iterations* times parse + find for all interesting modules *)
  let data = encode vs in
  let tags = List.init 50 (fun m -> m + 6) in
  (* TODO preserve one find result and compare (for correctness purposes) *)
  for _i = 0 to iterations do
    match VL.parse data with
    | Ok vs -> let _ = find_v ~tags vs in ()
    | Error (`Msg m) -> invalid_arg m
  done;
  (* this was just to get it hot (unclear if this is voodoo or not) *)
  let vf = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match VF.parse data with
    | Ok vs -> let _ = find_v ~tags vs in ()
    | Error (`Msg m) -> invalid_arg m
  done;
  let vf_vl = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match VL.parse data with
    | Ok vs -> let _ = find_v ~tags vs in ()
    | Error (`Msg m) -> invalid_arg m
  done;
  let vl_ve = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match VE.parse data with
    | Ok vs -> let _ = find_v ~tags vs in ()
    | Error (`Msg m) -> invalid_arg m
  done;
  let ve_mdf = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match MDF.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let mdf_mdl = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match MDL.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let mdl_mde = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match MDE.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let mde_mbf = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match MBF.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let mbf_mbl = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match MBL.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let mbl_mbe = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match MBE.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let mbe_vmf = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match VMF.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let vmf_vml = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match VML.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let vml_vme = Mtime_clock.now_ns () in
  for _i = 0 to iterations do
    match VME.parse data with
    | Ok m -> let _ = find_g ~tags m in ()
    | Error _ -> ()
  done;
  let vme = Mtime_clock.now_ns () in
  Printf.printf "%d iterations, input %d (%d bytes)\n VF %Luns\n VL %Luns\n VE %Luns\nMDF %Luns\nMDL %Luns\nMDE %Luns\nMBF %Luns\nMBL %Luns\nMBE %Luns\nVMF %Luns\nVML %Luns\nVME %Luns\n%!"
    iterations (List.length vs) (String.length data)
    (Int64.sub vf_vl vf)
    (Int64.sub vl_ve vf_vl)
    (Int64.sub ve_mdf vl_ve)
    (Int64.sub mdf_mdl ve_mdf)
    (Int64.sub mdl_mde mdf_mdl)
    (Int64.sub mde_mbf mdl_mde)
    (Int64.sub mbf_mbl mde_mbf)
    (Int64.sub mbl_mbe mbf_mbl)
    (Int64.sub mbe_vmf mbl_mbe)
    (Int64.sub vmf_vml mbe_vmf)
    (Int64.sub vml_vme vmf_vml)
    (Int64.sub vme vml_vme)

