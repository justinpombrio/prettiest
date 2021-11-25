-- This is my attempt at a major simplification of
-- Pretty Printing with Lazy Dequeues by Olaf Chitil.
--
-- (DOI:10.1145/1053468.1053473)
--
-- The idea is to compute the flatWidth (width of a document if it is rendered horizontally) of each
-- Doc at construction time. It's trivial to compute this at construction time, and it's enough
-- information to resolve each group will printing.

import System.Environment (getArgs)

-- From Appendix B of the paper: Doc implementation, and normalization

text :: String -> Doc
text s = Doc (Text s)

line :: Doc
line = Doc (Line)

infixr 6 <>
(<>) :: Doc -> Doc -> Doc
Doc l1 <> Doc l2 = Doc (l1 . l2)

group :: Doc -> Doc
group (Doc l) = Doc (Open . l . Close)

-- increment current left margin
nestMargin :: Int -> Doc -> Doc
nestMargin i (Doc l) = Doc (OpenNest (flip (const (+i))) . l . CloseNest)

-- set left margin to current column plus given increment
nestCol :: Int -> Doc -> Doc
nestCol i (Doc l) = Doc (OpenNest (const (+i)) . l . CloseNest)

doc2Tokens :: Doc -> Tokens
doc2Tokens (Doc f) = normalise (f Empty)

newtype Doc = Doc (Tokens -> Tokens)

data Tokens =
    Empty
  | Text String Tokens
  | Line Tokens
  | Open Tokens
  | Close Tokens
  | OpenNest (Int -> Int -> Int) Tokens
  | CloseNest Tokens

normalise :: Tokens -> Tokens
normalise = go id
  where
    go :: (Tokens -> Tokens) -> Tokens -> Tokens
    go co Empty = co Empty -- no opening brackets
    go co (Open ts) = go (co . open) ts
    go co (Close ts) = go (co . Close) ts
    go co (Line ts) = co . Line . go id $ ts
    go co (Text s ts) = Text s (go co ts)
    go co (OpenNest f ts) = OpenNest f (go co ts)
    go co (CloseNest ts) = CloseNest (go co ts)

    open (Close ts) = ts
    open ts = Open ts

-- Not from the paper: a simpler way to pretty print

-- A normalized Doc, in which each Group knows its flatWidth.
data DOC =
    EMPTY
  | TEXT String
  | LINE
  | GROUP DOC Int
  | NEST (Int -> Int -> Int) DOC
  | CONCAT DOC DOC

-- Precompute the flatWidth of each Doc.
precompute :: Tokens -> DOC
precompute ts = (\(d, _, _) -> d) (parse ts)
  where
    -- Parse up to the next unmatched close group or close nest.
    -- Return the group/nest, its flatWidth, and the remaining unparsed tokens.
    parse :: Tokens -> (DOC, Int, Tokens)
    parse Empty = (EMPTY, 0, error "unclosed group or nest")
    parse (Text s ts) =
      let (d, w, ts') = parse ts in (CONCAT (TEXT s) d, length s + w, ts')
    parse (Line ts) =
      let (d, w, ts') = parse ts in (CONCAT LINE d, 1 + w, ts')
    parse (Open ts) =
      let (d, w, ts') = parse ts
          (d', w', ts'') = parse ts' in
      (CONCAT (GROUP d w) d', w + w', ts'')
    parse (OpenNest f ts) =
      let (d, w, ts') = parse ts
          (d', w', ts'') = parse ts' in
      (CONCAT (NEST f d) d', w + w', ts'')
    parse (Close ts) = (EMPTY, 0, ts)
    parse (CloseNest ts) = (EMPTY, 0, ts)

-- Pretty printing is now simple: a group is horizontal if its flatWidth is less than or equal to
-- the remaining space on the line.
pretty :: Int -> Doc -> String
pretty w d = pp w w [(0, False, precompute $ doc2Tokens d)]
  where
    pp :: Int -> Int -> [(Int, Bool, DOC)] -> String
    pp w r [] = ""
    pp w r ((i, h, EMPTY) : ds) = pp w r ds
    pp w r ((i, h, TEXT s) : ds) = s ++ pp w (r - length s) ds
    pp w r ((i, True, LINE) : ds) = " " ++ pp w (w - i) ds
    pp w r ((i, False, LINE) : ds) = "\n" ++ (replicate i ' ') ++ pp w (w - i) ds
    pp w r ((i, h, GROUP d fw) : ds) = pp w r ((i, h || fw <= r, d) : ds)
    pp w r ((i, h, NEST f d) : ds) = pp w r ((f i (w - r), h, d) : ds)
    pp w r ((i, h, CONCAT x y) : ds) = pp w r ((i, h, x) : (i, h, y) : ds)

-- Testing. This `chitil` example is from the paper, in section 10.

chitilDoc :: Int -> Doc
chitilDoc 0 = text ""
chitilDoc n = group (text "*" <> line <> chitilDoc (n - 1))

chitil :: Doc
chitil = foldr1 (\x y -> x <> line <> y) $ take 500 $ repeat $ chitilDoc 200

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2 || args !! 0 /= "chitil"
  then putStrLn "Usage: ./lazy_dequeue 'chitil' size"
  else
    let size = (read $ args !! 1) :: Int
    in do
      putStrLn $ show $ length $ pretty size chitil
