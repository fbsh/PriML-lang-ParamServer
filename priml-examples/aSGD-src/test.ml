let dig_pow (n, p) =
  let 
    digits = List.map (fun d -> int_of_char d - int_of_char '0') (String.to_seq (string_of_int n) |> List.of_seq) in
  let power_sum = List.fold_left (fun acc (i, d) -> 
    acc + (int_of_float (float_of_int d ** float_of_int (p + i)))) 0 (List.mapi (fun i d -> (i, d)) digits) in
  if power_sum mod n = 0 then
    power_sum / n
  else
    -1
