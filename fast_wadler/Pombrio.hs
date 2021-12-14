-- An attempt at further simplification relative to lazy_dequeue.hs.
-- Optimized with help of Jonathan Sailor

-- This is my attempt at a major simplification of
-- Pretty Printing with Lazy Dequeues by Olaf Chitil.
--
-- (DOI:10.1145/1053468.1053473)
--
-- The idea is to _measure_ each Doc at construction time, determining enough information to resolve
-- choices without any lookahead.

module Pombrio (MDoc, nil, (<|>), (<>), nest, text, line, group, flatten, pretty) where
-- TODO: temp
type MDoc = Doc

{------------------------------------------------------------------------------}
{- Measurements                                                               -}
{------------------------------------------------------------------------------}

-- Int is flatLen         -- the minimum width if rendered flat
-- Maybe Int is suffixLen -- the width until the earliest possible newline, if any
data Measure = Measure Int (Maybe Int)

maxWidth = 256

emptyM :: Measure
emptyM = Measure 0 Nothing

flattenM :: Measure -> Measure
flattenM (Measure f s) = Measure f Nothing

-- Combine the measure of a Doc `x` and the measure of a Doc `y` to obtain the measure of `x <> y`.
addM :: Measure -> Measure -> Measure
addM m@(Measure f (Just s)) _ | s >= maxWidth = m
addM (Measure f (Just s)) (Measure f' _) = Measure (f + f') (Just s)
addM (Measure f Nothing) (Measure f' s') = Measure (f + f') suff
  where suff = case s' of
          Nothing -> Nothing
          Just s' -> Just (f + s')
--  addM (Measure f s) (Measure f' s') = Measure (f + f') (addSuffix f s s')
--    where
--    addSuffix _ Nothing  Nothing   = Nothing
--    addSuffix _ (Just s) Nothing   = Just s
--    addSuffix f Nothing  (Just s') = Just (f + s')
--    addSuffix f (Just s) (Just s') = Just s
--    addSuffix f (Just s) (Just s') = Just (s `min` (f + s'))

-- The width until the earliest possible newline, or end of document.
suffixLen :: Measure -> Int
suffixLen (Measure f Nothing) = f
suffixLen (Measure f (Just s)) = s
--suffixLen (Measure f (Just s)) = f `min` s

{------------------------------------------------------------------------------}
{- Pretty Printing                                                            -}
{------------------------------------------------------------------------------}

data Doc =
    Nil
  | Text String
  | Line
  | Nest Int Doc Measure
  | Flatten Doc Measure
  | Concat Doc Doc Measure
  | Choice Doc Doc Measure

measure :: Doc -> Measure
measure Nil = emptyM
measure (Text s) = Measure (length s) Nothing
measure Line = Measure 1 (Just 0)
measure (Nest i x m) = m
measure (Flatten x m) = m
measure (Concat x y m) = m
measure (Choice x y m) = m

group x = flatten x <|> x

nil = Nil
text s = Text s
line = Line
nest i x = Nest i x (measure x)
flatten x = Flatten x (flattenM (measure x))
x <> y = Concat x y (addM (measure x) (measure y))
x <|> y = Choice x y (measure y)

pretty :: Int -> Doc -> String
pretty w d = concat $ pp w 0 [(Just 0, emptyM, d)]
  where
    pp :: Int -> Int -> [(Maybe Int, Measure, Doc)] -> [String]
    pp w p [] = []
    pp w p ((_, _, Nil) : xs)              = pp w p xs
    pp w p ((_, _, Text s) : xs)           = s : pp w (p + length s) xs
    pp w p ((Nothing, m, Nest j x _) : ys) = pp w p ((Nothing, m, x) : ys)
    pp w p ((Just i,  m, Nest j x _) : ys) = pp w p ((Just (i + j), m, x) : ys)
    pp w p ((i, m, Flatten x _) : ys) = pp w p ((Nothing, m, x) : ys)
    pp w p ((i, m, Concat x y _) : zs) =
      pp w p ((i, addM (measure y) m, x) : (i, m, y) : zs)
    pp w p ((i, m, Choice x y _) : zs) =
      let itFits = p + (suffixLen (addM (measure x) m)) <= w
      in pp w p ((i, m, if itFits then x else y) : zs)
    pp w p ((i, m, Line) : xs) =
      case i of
        Nothing -> " " : pp w (p + 1) xs
        Just i  -> "\n" : replicate i ' ' : pp w i xs
