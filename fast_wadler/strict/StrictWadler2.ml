(* Strictly Pretty, Christian Lindig, 2000 *)
(* Slightly modified for readability by Justin *)

type doc =
  | DocNil
  | DocText of string
  | DocBreak of string
  | DocFlatten of doc
  | DocNest of int * doc
  | DocConcat of doc * doc
  | DocChoice of doc * doc

let empty       = DocNil
let text s      = DocText(s)
let break       = DocBreak(" ")
let breakWith s = DocBreak(s)
let flatten d   = DocFlatten(d)
let nest i x    = DocNest(i, x)
let (^^) x y    = DocConcat(x, y)
let (^?) x y    = DocChoice(x, y)
let group d     = flatten d ^? d

type mode = | Flat | Break

let rec fits (w: int) (parts: (int * mode * doc) list)
  = match parts with
  | _ when w < 0                 -> false
  | []                           -> true
  | (i, m, DocNil)          :: z -> fits w z
  | (i, m, DocConcat(x, y)) :: z -> fits w ((i, m, x) :: (i, m, y) :: z)
  | (i, m, DocNest(j, x))   :: z -> fits w ((i+j, m, x) :: z)
  | (i, m, DocText(s))      :: z -> fits (w - String.length s) z
  | (i, Flat, DocBreak(s))  :: z -> fits (w - String.length s) z
  | (i, Break, DocBreak(_)) :: z -> true (*impossible*)
  | (i, m, DocFlatten(x))   :: z -> fits w ((i, Flat, x) :: z)
  | (i, m, DocChoice(x, y)) :: z -> fits w ((i, m, y) :: z)

let rec format (w: int) (k: int) (parts: (int * mode * doc) list)
  = match parts with
  | []                           -> []
  | (i, m, DocNil)          :: z -> format w k z
  | (i, m, DocConcat(x, y)) :: z -> format w k ((i, m, x) :: (i, m, y) :: z)
  | (i, m, DocNest(j, x))   :: z -> format w k ((i+j, m, x) :: z)
  | (i, m, DocText(s))      :: z -> s :: format w (k + String.length s) z
  | (i, Flat, DocBreak(s))  :: z -> s :: format w (k + String.length s) z
  | (i, Break, DocBreak(s)) :: z -> "\n" :: String.make i ' ' :: format w i z
  | (i, m, DocFlatten(x))   :: z -> format w k ((i, Flat, x) :: z)
  | (i, m, DocChoice(x, y)) :: z -> if fits (w-k) ((i, m, x) :: z)
                                    then format w k ((i, m, x) :: z)
                                    else format w k ((i, m, y) :: z)

let pretty w doc = String.concat "" (format w 0 [0, Break, doc])

(*************
 * Test Case *
 *************)

let rec bigdoc n =
  if n == 0
  then text "<leaf/>"
  else group
    ((nest 2 (text "<branch>" ^^ break ^^ bigdoc (n-1) ^^ break ^^ bigdoc (n-1)))
      ^^ break ^^ text "</branch>");;

let rec json_string s = text ("\"" ^ s ^ "\"")
and json_number n = text (string_of_float n)
and json_list elems = match elems with
  | [] -> text "[]"
  | elems -> group (nest 4 (text "[" ^^ json_seq elems)
                    ^^ break ^^ text "]")
and json_dict pairs = match pairs with
  | [] -> text "{}"
  | elems -> group (nest 4 (text "{" ^^ json_seq (List.map json_pair pairs))
                    ^^ break ^^ text "}")
(* Not meant to be used directly. *)
and json_pair (key, value) = key ^^ text ": " ^^ value
and json_seq elems = match elems with
  | [elem] -> break ^^ elem
  | (elem :: elems) -> break ^^ elem ^^ text "," ^^ json_seq elems
  | [] -> failwith "unexpected empty list in json_seq";;

print_endline (pretty 80 (json_dict [
  (json_string "my favorite numbers", json_list [
      json_number 12.0;
      json_number 6.18281828;
      json_number 1.73 ]);
  (json_string "todo list", json_list [
      json_string "socks";
      json_string "jig"])
]));;
print_endline (pretty 80 (bigdoc 14));;
(* On laptop: 0.12s real time, 0.03s user time *)
