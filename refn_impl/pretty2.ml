open Hashtbl;;
open String;;
open List;;

type id = int
type width = int
type height = int
type overflow = int
type is_flat = bool

let global_id: id ref = ref 0
let next_id () = let id = !global_id in global_id := id + 1; id

type doc = doc_case * id

and doc_case =
  | Nil
  | Line
  | Text of string
  | Flat of doc
  | Nest of width * doc
  | Align of doc
  | Choice of doc * doc
  | Concat of doc * doc

let empty () = (Nil, next_id ())
let line () = (Line, next_id ())
let text s = (Text(s), next_id ())
let flat d = (Flat(d), next_id ())
let align d = (Align(d), next_id ())
let nest i d = (Nest(i, d), next_id ())
let (++) d1 d2 = (Concat(d1, d2), next_id ())
let (??) d1 d2 = (Choice(d1, d2), next_id ())

type mode = Flat | Break

(* mode * first * middle *)
type space = Space of mode * width * width

(* last * overflow * height *)
type measure = Measure of width * overflow * height

type measure_set = measure list

(* TODO! *)
let ms_union (ms1: measure_set) (ms2: measure_set): measure_set =
  ms1

let m_concat (Measure(l1, o1, h1)) (Measure(l2, o2, h2)) =
  Measure(l2, o1 + o2, h1 + h2)

type cache = ((id * space), measure_set) Hashtbl.t

let flatten_space  (Space(m, f, w)) = Space(Flat, f, w)
let indent_space i (Space(m, f, w)) = Space(m, f, w - i)
let align_space    (Space(m, f, w)) = Space(m, f, f)

let consume_space (Space(m, f, w)) (Measure(l, o, h)) = Space(m, l, w)

let rec measure (cache: cache) (space: space) (doc: doc): measure_set =
  let Space(m, f, w) = space in
  if f >= 0 && w >= 0
  then cached_measure cache space doc
  else
    let bounded_space = Space(m, max 0 f, max 0 w) in
    let ms = cached_measure cache bounded_space doc in
    let add_overflow (Measure(l, o, h)) =
      Measure(l, o + (max 0 (-f)) + (h - 1) * (max 0 (-w)), h) in
    List.map add_overflow ms

and cached_measure cache space (doc, id) =
  if Hashtbl.mem cache (id, space)
  then Hashtbl.find cache (id, space)
  else let m = calc_measure cache space (doc, id) in
       Hashtbl.add cache (id, space) m;
       m

and calc_measure cache space (doc, id) =
  match doc with
  | Nil     -> [Measure(0, 0, 0)]
  | Line    -> [Measure(0, 0, 1)]
  | Text(s) -> let len = String.length(s) in
               let Space(_, f, _) = space in
               [Measure(len, max 0 (len - f), 0)]
  | Flat(d)        -> measure cache (flatten_space space) (doc, id)
  | Nest(i, d)     -> measure cache (indent_space i space) (doc, id)
  | Align(d)       -> measure cache (align_space space) (doc, id)
  | Choice(d1, d2) -> ms_union (measure cache space d1)
                               (measure cache space d2)
  | Concat(d1, d2) -> 
    let process ms m1 =
      let remaining_space = consume_space space m1 in
      let ms2 = measure cache remaining_space d2 in
      let new_ms = List.map (fun m2 -> m_concat m1 m2) ms2 in
      ms_union ms new_ms
    in
    List.fold_left process [] (measure cache space d1)
