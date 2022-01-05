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
let nest i x    = DocNest(i, x)
let break       = DocBreak(" ")
let breakWith s = DocBreak(s)
let flatten d   = DocFlatten(d)
let (^^) x y    = DocConcat(x, y)
let (^?) x y    = DocChoice(x, y)
let group d     = flatten d ^? d

type sdoc =
  | SNil
  | SText of string * sdoc
  | SLine of int * sdoc

let rec sdocToList = function
  | SNil -> []
  | SText(s, d) -> s :: sdocToList d
  | SLine(i, d) -> "\n" :: String.make i ' ' :: sdocToList d

let sdocToString d = String.concat "" (sdocToList d)

type mode = | Flat | Break

let rec fits w = function
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

let rec format w k = function
  | []                           -> SNil
  | (i, m, DocNil)          :: z -> format w k z
  | (i, m, DocConcat(x, y)) :: z -> format w k ((i, m, x) :: (i, m, y) :: z)
  | (i, m, DocNest(j, x))   :: z -> format w k ((i+j, m, x) :: z)
  | (i, m, DocText(s))      :: z -> SText(s, format w (k + String.length s) z)
  | (i, Flat, DocBreak(s))  :: z -> SText(s, format w (k + String.length s) z)
  | (i, Break, DocBreak(s)) :: z -> SLine(i, format w i z)
  | (i, m, DocFlatten(x))   :: z -> format w k ((i, Flat, x) :: z)
  | (i, m, DocChoice(x, y)) :: z -> if fits (w-k) ((i, m, x) :: z)
                                    then format w k ((i, m, x) :: z)
                                    else format w k ((i, m, y) :: z)

let pretty w doc =
  let sdoc = format w 0 [0, Break, doc] in
  sdocToString sdoc

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
(* On laptop: 0.12s real time, 0.03s user time *)
