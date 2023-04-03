signature DIGEST = sig
  type t = string
  val string: string -> t
end

functor Make (Digest: DIGEST) = struct
  structure IntMap = IntInfMap.Map
  type 'a t = { map: (string * 'a) IntMap.map, interleave_count: int }

  fun make (interleave_count = 40) = { map = IntMap.empty, interleave_count = interleave_count }

  fun hash_val digested entry_fn =
    let
      fun slc i sw = IntInf.fromInt (ord (String.sub (digested, entry_fn i))) * IntInf.pow (2, sw)
    in
      slc 3 24 + slc 2 16 + slc 1 8 + slc 0 0
    end

  fun hash s = hash_val (Digest.string s) (fn x => x)

  fun add (key, value, weight = 1) m =
    let
      fun insert digested i map =
        IntMap.insert
          (map, hash_val digested (fn x => x + i * 4), (key, value))

      val factor = #interleave_count m * weight

      fun aux accum 0 = accum
        | aux accum j =
            let
              val f = insert (Digest.string (String.concat [key, "-", Int.toString j]))
            in
              aux (accum |> f 0 |> f 1 |> f 2) (j - 1)
            end
    in
      {map = aux (#map m) factor, interleave_count = #interleave_count m}
    end

  fun remove key m =
    {map = IntMap.filter (fn (_, (ks, _)) => ks <> key) (#map m), interleave_count = #interleave_count m}

  fun find key m =
    let
      val (l, data, r) = IntMap.split (#map m, hash key)
    in
      case data of
          SOME (_, x) => x
        | NONE =>
            if IntMap.isEmpty r
            then #2 (IntMap.minBinding l)
            else #2 (IntMap.minBinding r)
    end

  fun iter f m =
    let
      fun f' ki (ks, v) = f ki ks v
    in
      IntMap.app f' (#map m)
    end
end


(* Load the Make functor and Digest signature *)
structure SampleDigest: DIGEST = struct
  type t = string
  fun string s = s
end

structure SampleConsistentHash = Make(SampleDigest)

(* Test functions *)
fun testConsistentHashing () =
  let
    val ch = SampleConsistentHash.make ()
    val ch = SampleConsistentHash.add ("node1", "Node 1", 1) ch
    val ch = SampleConsistentHash.add ("node2", "Node 2", 1) ch
    val ch = SampleConsistentHash.add ("node3", "Node 3", 1) ch

    val keys = ["key1", "key2", "key3", "key4", "key5"]

    fun testKey key =
      let
        val node = SampleConsistentHash.find key ch
      in
        print (String.concat ["Key '", key, "' assigned to ", node, "\n"])
      end
  in
    List.app testKey keys
  end

(* Run the test *)
val _ = testConsistentHashing ()
