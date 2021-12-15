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

type space = {
  is_flat: bool;
  first: width;
  middle: width
}

type measure = {
  last: width;
  overflow: overflow;
  height: height
}

type measure_set = measure list

(* TODO! *)
let ms_union (ms1: measure_set) (ms2: measure_set): measure_set =
  ms1

let m_concat (m1: measure) (m2: measure): measure =
  { last = m2.last;
    overflow = m1.overflow + m2.overflow;
    height = m1.height + m2.height }

type cache = ((id * space), measure_set) Hashtbl.t

let flatten_space space =
  { is_flat = true; first = space.first; middle = space.first }

let indent_space i space =
  { is_flat = space.is_flat; first = space.first; middle = space.middle - i }

let align_space space =
  { is_flat = space.is_flat; first = space.first; middle = space.first }

let consume_space space measure =
  { is_flat = space.is_flat; first = measure.last; middle = space.middle }

let rec measure (cache: cache) (space: space) (doc: doc): measure_set =
  if space.first >= 0 && space.middle >= 0
  then cached_measure cache space doc
  else
    let bounded_space = {
      is_flat = space.is_flat;
      first = max 0 space.first;
      middle = max 0 space.middle;
    } in
    let ms = cached_measure cache bounded_space doc in
    let add_overflow m = {
      last = m.last;
      overflow = m.overflow
        + (max 0 (- space.first))
        + (m.height - 1) * (max 0 (- space.middle));
      height = m.height;
    } in
    List.map add_overflow ms

and cached_measure cache space (doc, id) =
  if Hashtbl.mem cache (id, space)
  then Hashtbl.find cache (id, space)
  else let m = calc_measure cache space (doc, id) in
       Hashtbl.add cache (id, space) m;
       m

and calc_measure cache space (doc, id) =
  match doc with
  | Nil     -> [{ last = 0; overflow = 0; height = 0 }]
  | Line    -> [{ last = 0; overflow = 0; height = 1 }]
  | Text(s) -> let len = String.length(s) in
    [{ last = len; overflow = max 0 (len - space.first); height = 0 }]
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
