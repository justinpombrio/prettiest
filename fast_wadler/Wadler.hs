module Wadler (DOC((:<|>)), flatten, nil, (<>), nest, text, line, group, pretty) where

-- This code is copy-pasted from the paper, with some type annotations added for clarity.
-- (A Prettier Printer, Philip Wadler)

import Data.Char (chr)

infixr 5 :<|>
infixr 6 :<>
infixr 6 <>

data DOC = NIL
  | DOC :<> DOC
  | NEST Int DOC
  | TEXT String
  | LINE
  | DOC :<|> DOC
  deriving Show

data Doc = Nil
  | String `Text` Doc
  | Int `Line` Doc

nil = NIL
x <> y = x :<> y
nest i x = NEST i x
text s = TEXT s
line = LINE
group x = flatten x :<|> x

-- O(n), where n is the DAG size.
-- Though remember: laziness.
flatten :: DOC -> DOC
flatten NIL = NIL
flatten (x :<> y) = flatten x :<> flatten y
flatten (NEST i x) = NEST i (flatten x)
flatten (TEXT s) = TEXT s
flatten LINE = TEXT " "
flatten (x :<|> y) = flatten x

layout :: Doc -> String
layout Nil = ""
layout (s `Text` x) = s ++ layout x
layout (i `Line` x) = '\n' : copy i ' ' ++ layout x
  where
    copy i x = [ x | _ <- [1..i] ]

best :: Int -> Int -> DOC -> Doc
best w k x = be w k [(0,x)]

-- Most of these are O(1 + #flats)
be :: Int -> Int -> [(Int, DOC)] -> Doc
be w k [] = Nil
be w k ((i,NIL):z) = be w k z
be w k ((i,x :<> y):z) = be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z) = be w k ((i+j,x):z)
be w k ((i,TEXT s):z) = s `Text` be w (k+length s) z
be w k ((i,LINE):z) = i `Line` be w i z
-- laziness is important here!
-- O(w * ?)
be w k ((i,x :<|> y):z) = better w k (be w k ((i,x):z)) (be w k ((i,y):z))

-- O(w)
better :: Int -> Int -> Doc -> Doc -> Doc
better w k x y = if fits (w-k) x then x else y

-- O(w)
fits :: Int -> Doc -> Bool
fits w x | w < 0 = False
fits w Nil = True
fits w (s `Text` x) = fits (w - length s) x
fits w (i `Line` x) = True

pretty :: Int -> DOC -> String
pretty w x = layout (best w 0 x)
