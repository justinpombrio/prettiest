-- An attempt at further simplification relative to lazy_dequeue.hs.

-- This is my attempt at a major simplification of
-- Pretty Printing with Lazy Dequeues by Olaf Chitil.
--
-- (DOI:10.1145/1053468.1053473)
--
-- The idea is to _measure_ each Doc at construction time, determining enough information to resolve
-- choices without any lookahead.

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intercalate)

{-----------------------------------------------------------------------------}
{- Measurements                                                              -}
{-----------------------------------------------------------------------------}

-- Int is flatLen         -- the minimum width if rendered horizontally
-- Maybe Int is suffixLen -- the width until the earliest possible newline
data Measure = Measure Int (Maybe Int)

emptyMeasure :: Measure
emptyMeasure = Measure 0 Nothing

-- Combine the measure of a Doc `x` and the measure of a Doc `y` to obtain the measure of `x <> y`.
addMeasure :: Measure -> Measure -> Measure
addMeasure (Measure f s) (Measure f' s') = Measure (f + f') (addSuffix f s s')

addSuffix _ Nothing  Nothing   = Nothing
addSuffix _ (Just s) Nothing   = Just s
addSuffix f Nothing  (Just s') = Just (f + s')
addSuffix f (Just s) (Just s') = Just (s `min` (f + s'))

-- The minimum width, if rendered flat (horizontally).
flatLen :: Measure -> Int
flatLen (Measure f _) = f

-- The width until the earliest possible newline, or end of document.
suffixLen :: Measure -> Int
suffixLen (Measure f Nothing) = f
suffixLen (Measure f (Just s)) = f `min` s

{-----------------------------------------------------------------------------}
{- Pretty Printing                                                           -}
{-----------------------------------------------------------------------------}

data Doc =
    Empty
  | Text String
  | Line
  | Nest Int Doc Measure
  | Group Doc Measure
  | Concat Doc Doc Measure

measure :: Doc -> Measure
measure Empty          = Measure 0 Nothing
measure (Text s)       = Measure (length s) Nothing
measure Line           = Measure 1 (Just 0)
measure (Nest _ _ m)   = m
measure (Concat _ _ m) = m
measure (Group _ m)    = m

nil = Empty
text s = Text s
line = Line
nest i x = Nest i x (measure x)
group x = Group x (measure x)
x <> y = Concat x y (addMeasure (measure x) (measure y))

pretty :: Int -> Doc -> String
pretty w d = concat $ pp w 0 [(0, False, emptyMeasure, d)]
  where
    pp :: Int -> Int -> [(Int, Bool, Measure, Doc)] -> [String]
    pp w p [] = []
    pp w p ((_, _, _, Empty) : xs)      = pp w p xs
    pp w p ((_, _, _, Text s) : xs)     = s : pp w (p + length s) xs
    pp w p ((i, h, m, Nest j x _) : xs) = pp w p ((i + j, h, m, x) : xs)
    pp w p ((i, h, m, Group x m') : xs) =
      let fits = p + flatLen m' + suffixLen m <= w
      in pp w p ((i, h || fits, m, x) : xs)
    pp w p ((i, h, m, Concat x y _) : xs) =
      pp w p ((i, h, addMeasure (measure y) m, x) : (i, h, m, y) : xs)
    pp w p ((i, h, m, Line) : xs)      =
      if h
      then " " : pp w (p + 1) xs
      else ("\n" ++ replicate i ' ') : pp w 0 xs

{-----------------------------------------------------------------------------}
{- Testing. The `chitil` example is from the paper, in section 10.           -}
{-----------------------------------------------------------------------------}

chitilDoc :: Int -> Doc
chitilDoc 0 = text ""
chitilDoc n = group (text "*" <> line <> chitilDoc (n - 1))

chitil :: Doc
chitil = foldr1 (\x y -> x <> line <> y) $ take 500 $ repeat $ chitilDoc 200

incr :: Int -> Doc
incr 0 = text "leaf(size = 0)"
incr n = group (text ("branch(size = " ++ show (2 ^ n) ++ "):")
                       <> nest 4 (line <> subdoc <> line <> subdoc)
                       <> line <> text "end")
  where subdoc = incr (n - 1)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then do
    putStrLn "Usage: ./linear_time [DOC] size"
    putStrLn "where DOC is 'chitil' or 'incremental'"
  else
    let which = args !! 0
        size = (read $ args !! 1) :: Int
    in case which of
      "chitil"         -> putStrLn $ show $ length $ pretty size chitil
      "incremental"    -> putStrLn $ intercalate "\n" $ take 10 $ splitOn "\n"
                          $ pretty 80 (incr size)
