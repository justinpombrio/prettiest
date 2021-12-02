-- This code is copy-pasted from the paper, with some type annotations added for clarity.
-- (A Prettier Printer, Philip Wadler)

import System.Environment (getArgs)
---

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

-----------------------------------------
-- Code after here is from Justin Pombrio

-- A counter example that proves that the condition required for using :<|> given in the paper is
-- incorrect. Though the correct condition is _also_ obeyed by the constructs in the paper that use
-- :<|> (such as `group`), so the proofs in the paper stand for the DOC constructors given in the
-- paper. You just need to be careful if you extend the printer by adding a new constructor that
-- uses :<|>.

counterEx :: DOC
counterEx = line <> group ab
  where ab = text "a" <> line <> text "b"

counterEx' :: DOC
counterEx' = (line <> flatten ab) :<|> (line <> ab)
  where ab = text "a" <> line <> text "b"

runCounterExamples :: IO ()
runCounterExamples = do
  putStrLn "------counterEx"
  putStrLn $ pretty 2 counterEx
  putStrLn "------counterEx'"
  putStrLn $ pretty 2 counterEx'
  putStrLn "------"

-- Various DOCs that print slowly, some more devious than others.

huge :: Int -> DOC
huge 0 = nil
huge n = let d = huge (n - 1) in d <> d

antagonistic :: Int -> DOC
antagonistic 0 = text "line"
antagonistic n = group (antagonistic (n - 1) <> line <> text "line")

nestedLists :: Int -> DOC
nestedLists 0 = text "[]"
nestedLists n = group (text "[" <> (nestedLists (n - 1)) <> text "]")

chitilDoc :: Int -> DOC
chitilDoc 0 = text ""
chitilDoc n = group (text "*" <> line <> chitilDoc (n - 1))

chitil :: DOC
chitil = foldr1 (\x y -> x <> line <> y) $ take 500 $ repeat $ chitilDoc 200

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then do
    putStrLn "Usage: ./prettier [DOC] size"
    putStrLn "where DOC is 'huge', 'antagonistic', 'nestedLists', or 'chitil'"
  else
    let which = args !! 0
        size = (read $ args !! 1) :: Int
    in case which of
      "huge"         -> putStrLn $ pretty 10 (huge size)
      "nestedLists"  -> putStrLn $ pretty (2 * size) (nestedLists size)
      "antagonistic" -> putStrLn $ pretty 10 (antagonistic size)
      "chitil"       -> putStrLn $ show $ length $ pretty size chitil
      _              -> error "DOC not recognized"
