-- An attempt at further simplification relative to lazy_dequeue.hs.

-- This is my attempt at a major simplification of
-- Pretty Printing with Lazy Dequeues by Olaf Chitil.
--
-- (DOI:10.1145/1053468.1053473)
--
-- The idea is to _measure_ each Doc at construction time, determining enough information to resolve
-- choices without any lookahead.

module Pombrio (MDoc, nil, (<|>), (<>), nest, text, line, group, flatten, pretty) where

{------------------------------------------------------------------------------}
{- Measurements                                                               -}
{------------------------------------------------------------------------------}

-- Int is flatLen         -- the minimum width if rendered flat
-- Maybe Int is suffixLen -- the width until the earliest possible newline, if any
data Measure = Measure Int (Maybe Int)

emptyM :: Measure
emptyM = Measure 0 Nothing

flattenM :: Measure -> Measure
flattenM (Measure f s) = Measure f Nothing

-- Combine the measure of a Doc `x` and the measure of a Doc `y` to obtain the measure of `x <> y`.
addM :: Measure -> Measure -> Measure
addM (Measure f s) (Measure f' s') = Measure (f + f') (addSuffix f s s')
  where
    addSuffix _ Nothing  Nothing   = Nothing
    addSuffix _ (Just s) Nothing   = Just s
    addSuffix f Nothing  (Just s') = Just (f + s')
    addSuffix f (Just s) (Just s') = Just (s `min` (f + s'))

-- The width until the earliest possible newline, or end of document.
suffixLen :: Measure -> Int
suffixLen (Measure f Nothing) = f
suffixLen (Measure f (Just s)) = f `min` s

{------------------------------------------------------------------------------}
{- Pretty Printing                                                            -}
{------------------------------------------------------------------------------}

type MDoc = (Doc, Measure)

data Doc =
    Empty
  | Text String
  | Line
  | Nest Int MDoc
  | Flatten MDoc
  | MDoc :<> MDoc
  | MDoc :<|> MDoc

nil = (Empty, emptyM)
text s = (Text s, Measure (length s) Nothing)
line = (Line, Measure 1 (Just 0))
nest i (x, xm) = (Nest i (x, xm), xm)
group x = flatten x <|> x
flatten (x, mx) = (Flatten (x, mx), flattenM mx)
(x, mx) <> (y, my) = ((x, mx) :<> (y, my), addM mx my)
(x, mx) <|> (y, my) = ((x, mx) :<|> (y, my), my)

pretty :: Int -> MDoc -> String
pretty w d = concat $ pp w 0 [(0, False, emptyM, fst d)]
  where
    pp :: Int -> Int -> [(Int, Bool, Measure, Doc)] -> [String]
    pp w p [] = []
    pp w p ((_, _, _, Empty) : xs)           = pp w p xs
    pp w p ((_, _, _, Text s) : xs)          = s : pp w (p + length s) xs
    pp w p ((i, h, m, Nest j (x, mx)) : ys)  = pp w p ((i + j, h, m, x) : ys)
    pp w p ((i, h, m, Flatten (x, mx)) : ys) = pp w p ((i, True, flattenM m, x) : ys)
    pp w p ((i, h, m, (x, mx) :<|> (y, my)) : zs) =
      let itFits = p + (suffixLen (addM mx m)) <= w
      in pp w p ((i, h, m, if itFits then x else y) : zs)
    pp w p ((i, h, m, (x, mx) :<> (y, my)) : zs) =
      pp w p ((i, h, addM my m, x) : (i, h, m, y) : zs)
    pp w p ((i, h, m, Line) : xs) =
      if h
      then " " : pp w (p + 1) xs
      else ("\n" ++ replicate i ' ') : pp w i xs
