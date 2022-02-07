
(****************
 * Measurements *
 ****************)

type measure = {
  flat : int;           (* the len if rendered flat *)
  nonflat : int option; (* the len until the earliest possible newline, if any *)
}

let empty_m = { flat = 0; nonflat = None }
let flatten_m { flat; nonflat } = { flat = flat; nonflat = None }

let add_suff f s1 s2 = match (s1, s2) with
  | (None, None)     -> None
  | (None, Some(s2)) -> Some(f + s2)
  | (Some(s1), _)    -> Some(s1)

(* Combine the measure of a Doc `x` and the measure of a Doc `y` to obtain the
 * measure of `x ^^ y`.  *)
let add_m m1 m2 = {
  flat = m1.flat + m2.flat;
  nonflat = add_suff m1.flat m1.nonflat m2.nonflat;
}

(************************
 * Data Representations *
 ************************)

(* Designed to match Lindig's Strictly Pretty for fair comparison. *)

type doc =
  | DocNil
  | DocText of string
  | DocBreak of string
  | DocFlatten of doc * measure
  | DocNest of int * doc * measure
  | DocConcat of doc * doc * measure
  | DocChoice of doc * doc * measure

let measure d = match d with
  | DocNil             -> empty_m
  | DocText(s)         -> { flat = String.length s; nonflat = None }
  | DocBreak(s)        -> { flat = String.length s; nonflat = Some(0) }
  | DocFlatten(_, m)   -> m
  | DocNest(_, _, m)   -> m
  | DocConcat(_, _, m) -> m
  | DocChoice(_, _, m) -> m

let empty       = DocNil
let text s      = DocText(s)
let break       = DocBreak(" ")
let breakWith s = DocBreak(s)
let flatten d   = DocFlatten(d, flatten_m (measure d))
let nest i x    = DocNest(i, x, measure x)
let (^^) x y    = DocConcat(x, y, add_m (measure x) (measure y))
let (^?) x y    = DocChoice(x, y, measure y)
let group d     = flatten d ^? d

(* The width until the earliest possible newline, or end of document. *)
let suffix_len = function
  | { flat; nonflat = None } -> flat
  | { flat; nonflat = Some(s) } -> s

(*******************
 * Pretty Printing *
 *******************)

type mode = | Flat | Ind of int

let rec format w k = function
  | []                                 -> []
  | (i, m, DocNil)                :: z -> format w k z
  | (i, m, DocText(s))            :: z -> s :: format w (k + String.length s) z
  | (Flat, m, DocBreak(s))        :: z -> s :: format w (k + String.length s) z
  | (Ind(i), m, DocBreak(s))      :: z -> "\n" :: String.make i ' ' :: format w i z
  | (Flat, m, DocNest(j, x, _))   :: z -> format w k ((Flat, m, x) :: z)
  | (Ind(i), m, DocNest(j, x, _)) :: z -> format w k ((Ind(i+j), m, x) :: z)
  | (i, m, DocConcat(x, y, _))    :: z ->
    let mx = add_m (measure y) m in
    format w k ((i, mx, x) :: (i, m, y) :: z)
  | (i, m, DocFlatten(x, _))      :: z -> format w k ((Flat, m, x) :: z)
  | (i, m, DocChoice(x, y, _))    :: z ->
    if k + suffix_len (add_m (measure x) m) <= w
    then format w k ((i, m, x) :: z)
    else format w k ((i, m, y) :: z)

let pretty w doc = String.concat "" (format w 0 [Ind(0), measure doc, doc])

(*************
 * Test Case *
 *************)

let rec bigdoc n =
  if n == 0
  then text "<leaf/>"
  else group
    ((nest 2 (text "<branch>" ^^ break ^^ bigdoc (n-1) ^^ break ^^ bigdoc (n-1)))
      ^^ break ^^ text "</branch>");;

print_endline (pretty 80 (bigdoc 14));;
(* On laptop: 0.13s real time, 0.04s user time *)
