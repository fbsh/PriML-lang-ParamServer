let xo (s: string): bool =
  let s = String.lowercase_ascii s in
  let x_count = ref 0 in
  let o_count = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | 'x' -> x_count := !x_count + 1
    | 'o' -> o_count := !o_count + 1
    | _ -> ()
  done;
  !x_count = !o_count
