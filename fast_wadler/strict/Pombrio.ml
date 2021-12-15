
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

(* Combine the measure of a Doc `x` and the measure of a Doc `y` to obtain the measure of `x <> y`.  *)
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
  | DocNest of int * doc * measure
  | DocGroup of doc * measure
  | DocCons of doc * doc * measure

let strlen  = String.length
let nl      = "\n"

let measure d = match d with
  | DocNil           -> empty_m
  | DocText(s)       -> { flat = strlen s; nonflat = None }
  | DocBreak(s)      -> { flat = strlen s; nonflat = Some(0) }
  | DocNest(_, _, m) -> m
  | DocGroup(_, m)   -> m
  | DocCons(_, _, m) -> m

let empty       = DocNil
let text s      = DocText(s)
let break       = DocBreak(" ")
let breakWith s = DocBreak(s)
let nest i x    = DocNest(i, x, measure x)
let (^^) x y    = DocCons(x, y, add_m (measure x) (measure y))
let group d     = DocGroup(d, measure d)

type sdoc =
  | SNil
  | SText of string * sdoc
  | SLine of int * sdoc

let rec sdocToString = function
  | SNil        -> ""
  | SText(s, d) -> s ^ sdocToString d
  | SLine(i, d) -> let prefix = String.make i ' '
                   in nl ^ prefix ^ sdocToString d

(* The width until the earliest possible newline, or end of document. *)
let suffix_len = function
  | { flat; nonflat = None } -> flat
  | { flat; nonflat = Some(s) } -> s

(*******************
 * Pretty Printing *
 *******************)

let rec format w k = function
  | []                                  -> SNil
  | (i, m, DocNil)                 :: z -> format w k z
  | (i, m, DocText(s))             :: z -> SText(s, format w (k + strlen s) z)
  | (None, m, DocBreak(s))         :: z -> SText(s, format w (k + strlen s) z)
  | (Some(i), m, DocBreak(s))      :: z -> SLine(i, format w i z)
  | (None, m, DocNest(j, x, _))    :: z -> format w k ((None, m, x) :: z)
  | (Some(i), m, DocNest(j, x, _)) :: z -> format w k ((Some(i+j), m, x) :: z)
  | (i, m, DocCons(x, y, _))       :: z ->
    let mx = add_m (measure y) m in
    format w k ((i, mx, x) :: (i, m, y) :: z)
  | (i, m, DocGroup(x, _))         :: z ->
    if k + suffix_len (add_m (flatten_m (measure x)) m) <= w
    then format w k ((None, m, x) :: z)
    else format w k ((i, m, x) :: z)

let rec bigdoc n =
  if n == 0
  then text "<leaf/>"
  else group
    ((nest 2 (text "<branch>" ^^ break ^^ bigdoc (n-1) ^^ break ^^ bigdoc (n-1)))
     ^^ break ^^ text "</branch>")

let pretty w doc =
  let sdoc = format w 0 [(Some(0), empty_m, doc)] in
  sdocToString sdoc

;;
print_endline (pretty 80 (bigdoc 12))
