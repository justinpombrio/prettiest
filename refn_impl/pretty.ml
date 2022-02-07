open Hashtbl;;
open String;;
open List;;

type width = int
type height = int
type overflow = int
type is_flat = bool

(* O(n(w+o)^3) *)
let hard_overflow_limit: width = -40

let global_id: id ref = ref 0
let next_id () = let id = !global_id in global_id := id + 1; id

type id = int

type doc = doc_case * id
and doc_case =
  | Empty
  | Text of string
  | Newline
  | Nest of width * doc
  | Align of doc
  | Concat of doc * doc
  | Choice of doc * doc
  | Flat of doc

let empty ()     = (Empty,          next_id ())
let line ()      = (Newline,        next_id ())
let text s       = (Text(s),        next_id ())
let flat d       = (Flat(d),        next_id ())
let nest i d     = (Nest(i, d),     next_id ())
let align d      = (Align(d),       next_id ())
let (^^) d1 d2   = (Concat(d1, d2), next_id ())
let (^?) d1 d2   = (Choice(d1, d2), next_id ())
let choice d1 d2 = (Choice(d1, d2), next_id ())

type mode = Flat | Break

(* Printing function. Take the strings printed so far, and an indentation, and
 * return an extended list of strings to print. The list of strings is in
 * reverse order, because we want to append "to the end". *)
type formatter = string list -> int -> string list

(* - mode (flat or not)
 * - available width for first line
 * - available width for further lines *)
type space = Space of mode * width * width

(* - space remaining on the last line (good)
 * - amount of overflow (very bad)
 * - number of newlines (bad)
 * - printing function *)
type measure = Measure of width * overflow * height * formatter

(* INVARIANT: measures are sorted in order of
 * ascending last and ascending (overflow, height) *)
type measure_set = measure list

(* Given the measure of doc x and the measure of doc y,
 * determine the measure of x ^? y. *)
let rec union_ms (ms1: measure_set) (ms2: measure_set): measure_set =
  match (ms1, ms2) with
  | ([], ms2) -> ms2
  | (ms1, []) -> ms1
  | (m1 :: ms1, m2 :: ms2) ->
      let Measure(l1, o1, h1, _) = m1
      and Measure(l2, o2, h2, _) = m2 in
      (* m1 is just worse; discard it *)
      if l1 <= l2 && (o1, h1) >= (o2, h2)
      then union_ms ms1 (m2 :: ms2)
      (* m2 is just worse; discard it *)
      else if l2 <= l1 && (o2, h2) >= (o1, h1)
      then union_ms (m1 :: ms1) ms2
      (* determine the order that obeys the invariant *)
      else if l1 <= l2
      then m1 :: (union_ms ms1 (m2 :: ms2))
      else m2 :: (union_ms (m1 :: ms1) ms2)

(* Given the measure of doc x and the measure of doc y,
 * determine the measure of x ^^ y. *)
let concat_m (Measure(l1, o1, h1, p1)) (Measure(l2, o2, h2, p2)) =
  Measure(l2, o1 + o2, h1 + h2, fun s i -> p2 (p1 s i) i)

(* Given the measure of doc x, determine the measure of Nest(j, x). *)
let indent_m j (Measure(l, o, h, p)) =
  Measure(l, o, h, fun s i -> p s (i + j))

type cache = ((id * space), measure_set) Hashtbl.t

let flatten_space  (Space(m, f, w)) = Space(Flat, f, w)
let indent_space i (Space(m, f, w)) = Space(m, f, w-i)

let consume_space (Space(m, f, w)) (Measure (l, o, h, _))
  = Space(m, l, w)

let rec measure cache space (doc, id) =
  let Space(m, f, w) = space in
  if f < hard_overflow_limit || w < hard_overflow_limit
  then []
  else if Hashtbl.mem cache (id, space)
  then Hashtbl.find cache (id, space)
  else let ms = calc_measure cache space (doc, id) in
       Hashtbl.add cache (id, space) ms;
       ms

and calc_measure cache space (doc, id) =
  let Space(m, f, w) = space in
  match doc with
  | Empty   -> [Measure(f, 0, 0,
                fun s i -> s)]
  | Newline -> if m == Flat
               then []
               else [Measure(w, max 0 (- w), 1,
                     fun s i -> String.make i ' ' :: "\n" :: s)]
  | Text(t) -> let len = String.length(t) in
               [Measure(f - len, max 0 (len - f), 0,
                fun s i -> t :: s)]
  | Flat(d) -> measure cache (flatten_space space) d
  | Nest(i, d) -> map (indent_m i)
    (measure cache (indent_space i space) d)
  | Align(d) -> map (indent_m (w - f))
    (measure cache (indent_space (w - f) space) d)
  | Choice(d1, d2) -> union_ms (measure cache space d1)
                               (measure cache space d2)
  | Concat(d1, d2) -> 
    let process ms m1 =
      let remaining_space = consume_space space m1 in
      let ms2 = measure cache remaining_space d2 in
      let new_ms = List.map (fun m2 -> concat_m m1 m2) ms2 in
      union_ms ms new_ms
    in
    List.fold_left process [] (measure cache space d1)

let pretty (doc: doc) (w: width): string =
  let cache = Hashtbl.create 1000 in
  let ms = measure cache (Space(Break, w, w)) doc in
  if List.length ms == 0
  then "[IMPOSSIBLE]"
  else let Measure(_, _, _, p) = List.hd ms in
       print_endline ("measure set size " ^ string_of_int (List.length ms));
       let strings = List.rev (p [] 0) in
       String.concat "" strings

(***********
 * Testing *
 ***********)
  
let rec bigdoc n: doc =
  if n == 0
  then text "<leaf/>"
  else
    let subdoc1 = bigdoc (n - 1)
    and subdoc2 = bigdoc (n - 1) in
       ((nest 2 (text "<branch>" ^^ line ()
                 ^^ subdoc1 ^^ line ()
                 ^^ subdoc2) ^^ line ())
         ^^ text "</branch>")
    ^? (text "<branch>" ^^ text " "
        ^^ flat(subdoc1) ^^ text " "
        ^^ flat(subdoc2) ^^ text " "
        ^^ text "</branch>");;

(* 1.75 with ocamlopt on laptop *)
(* 2.25 with ocaml -O2 on laptop *)
let doc = bigdoc 14 in
print_endline (pretty doc 80);;
