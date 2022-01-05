
(* For debugging *)
let string_of_m (Measure(l, o, h, _)) =
  "M(" ^ string_of_int l
  ^ ", " ^ string_of_int o
  ^ ", " ^ string_of_int h
  ^ ")"

(* For debugging *)
let string_of_mode mode = match mode with
  | Flat  -> "F"
  | Break -> "B"

(* For debugging *)
let string_of_s (Space(m, f, w)) =
  "S(" ^ string_of_mode m
  ^ ", " ^ string_of_int f
  ^ ", " ^ string_of_int w
  ^ ")"

(* For debugging *)
let rec string_of_ms ms = match ms with
  | [] -> "[]"
  | (m :: ms) -> string_of_m m ^ " :: " ^ string_of_ms ms

(* For debugging *)
let union_ms_debug ms1 ms2 =
  let result = union_ms ms1 ms2 in
  print_endline ("union "
    ^ string_of_ms ms1 
    ^ " & "
    ^ string_of_ms ms2
    ^ " = "
    ^ string_of_ms result);
  result
