module IntMap = Map.Make (Int64)

module type DIGEST =
sig
  type t = string
  val string: string -> t
end

module Make (Digest: DIGEST) = struct
  type +'a t = {
    map: (string * 'a) IntMap.t;
    interleave_count: int;
  }

  let make ?(interleave_count = 40) () =
    {map = IntMap.empty; interleave_count}

  let hash_val digested entry_fn =
    let slc i sw =
      Int64.shift_left (Int64.of_int (int_of_char digested.[entry_fn i])) sw
    and (lor) = Int64.logor in
    (slc 3 24) lor (slc 2 16) lor (slc 1 8) lor (slc 0 0)

  let hash s =
    hash_val (Digest.string s) (fun x -> x)

  let add ?(weight = 1) key value m =
    let insert digested i map =
      IntMap.add
        (hash_val digested (fun x -> x + i * 4))
        (key, value)
        map
    and factor = m.interleave_count * weight in
    let rec aux accum = function
      | x when x = factor -> accum
      | j ->
        let f = Printf.sprintf "%s-%d" key j |> Digest.string |> insert in
        aux
          (accum |> f 0 |> f 1 |> f 2)
          (succ j)
    in
    {m with map = aux m.map 0}

  let remove key m =
    {m with map = IntMap.filter (fun _ (ks, _) -> ks <> key) m.map}

  let find key m =
    let l, data, r = IntMap.split (hash key) m.map in
    match data with
    | Some (_, x) -> x
    | None when IntMap.is_empty r -> IntMap.min_binding l |> snd |> snd
    | None -> IntMap.min_binding r |> snd |> snd

  let iter f m =
    let f' ki (ks, v) = f ki ks v in
    IntMap.iter f' m.map

end