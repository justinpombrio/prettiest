(* Strictly Pretty, Christian Lindig, 2000 *)

type doc =
  | DocNil
  | DocCons of doc * doc
  | DocText of string
  | DocNest of int * doc
  | DocBreak of string
  | DocGroup of doc

let strlen  = String.length
let nl      = "\n"

let (^^) x y    = DocCons(x, y)
let empty       = DocNil
let text s      = DocText(s)
let nest i x    = DocNest(i, x)
let break       = DocBreak(" ")
let breakWith s = DocBreak(s)
let group d     = DocGroup(d)

type sdoc =
  | SNil
  | SText of string * sdoc
  | SLine of int * sdoc

let rec sdocToString = function
  | SNil        -> ""
  | SText(s, d) -> s ^ sdocToString d
  | SLine(i, d) -> let prefix = String.make i ' '
                   in nl ^ prefix ^ sdocToString d

type mode = | Flat | Break

let rec fits w = function
  | _ when w < 0                 -> false
  | []                           -> true
  | (i, m, DocNil)          :: z -> fits w z
  | (i, m, DocCons(x, y))   :: z -> fits w ((i, m, x) :: (i, m, y) :: z)
  | (i, m, DocNest(j, x))   :: z -> fits w ((i+j, m, x) :: z)
  | (i, m, DocText(s))      :: z -> fits (w - strlen s) z
  | (i, Flat, DocBreak(s))  :: z -> fits (w - strlen s) z
  | (i, Break, DocBreak(_)) :: z -> true (*impossible*)
  | (i, m, DocGroup(x))     :: z -> fits w ((i, Flat, x) :: z)

let rec format w k = function
  | []                           -> SNil
  | (i, m, DocNil)          :: z -> format w k z
  | (i, m, DocCons(x, y))   :: z -> format w k ((i, m, x) :: (i, m, y) :: z)
  | (i, m, DocNest(j, x))   :: z -> format w k ((i+j, m, x) :: z)
  | (i, m, DocText(s))      :: z -> SText(s, format w (k + strlen s) z)
  | (i, Flat, DocBreak(s))  :: z -> SText(s, format w (k + strlen s) z)
  | (i, Break, DocBreak(s)) :: z -> SLine(i, format w i z)
  | (i, m, DocGroup(x))     :: z -> if fits (w-k) ((i, Flat, x) :: z)
                                    then format w k ((i, Flat, x) :: z)
                                    else format w k ((i, Break, x) :: z)

let rec bigdoc n =
  if n == 0
  then text "<leaf/>"
  else group
    ((nest 2 (text "<branch>" ^^ break ^^ bigdoc (n-1) ^^ break ^^ bigdoc (n-1)))
     ^^ break ^^ text "</branch>")

let pretty w doc =
  let sdoc = format w 0 [0, Flat, DocGroup(doc)] in
  sdocToString sdoc

;;
print_endline (pretty 80 (bigdoc 12))
