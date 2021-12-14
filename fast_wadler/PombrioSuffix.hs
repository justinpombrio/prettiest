-- An attempt at further simplification relative to lazy_dequeue.hs.

-- This is my attempt at a major simplification of
-- Pretty Printing with Lazy Dequeues by Olaf Chitil.
--
-- (DOI:10.1145/1053468.1053473)
--
-- The idea is to _measure_ each Doc at construction time, determining enough information to resolve
-- choices without any lookahead.

module Pombrio (MDoc, nil, (<|>), (<+>), nest, text, line, group, flatten, pretty) where

{------------------------------------------------------------------------------}
{- Pretty Printing                                                            -}
{------------------------------------------------------------------------------}

type MDoc = Bool -> (Int -> Bool) -> (Doc, Int -> Bool)

data Doc =
    Nil
  | Text String
  | Line
  | Nest Int (Doc, Int -> Bool)
  | Flatten (Doc, Int -> Bool)
  | Concat (Doc, Int -> Bool) (Doc, Int -> Bool)
  | Choice (Doc, Int -> Bool) (Doc, Int -> Bool)

nil :: MDoc
text :: String -> MDoc
line :: MDoc
nest :: Int -> MDoc -> MDoc
flatten :: MDoc -> MDoc
(<+>) :: MDoc -> MDoc -> MDoc
(<|>) :: MDoc -> MDoc -> MDoc

nil       = \h m -> (Nil, m)
text s    = \h m -> (Text s, \w -> length s <= w && m (w - length s))
line      = \h m -> if h then (Line, \w -> m (w - 1)) else (Line, \w -> 0 <= w)
nest i x  = \h m -> let (x', mx) = x h m in (Nest i (x', mx), mx)
flatten x = \h m -> let (x', mx) = x True m in (Flatten (x', mx), mx)

x <+> y = \h m ->
  let (x', mx) = x h my
      (y', my) = y h m
  in (Concat (x', mx) (y', my), mx)

x <|> y = \h m ->
  let (x', mx) = x h m
      (y', my) = y h m
  in (Choice (x', mx) (y', my), my)

group x = flatten x <|> x

pretty :: Int -> MDoc -> String
pretty w d = concat $ pp w 0 [(Just 0, d False (\w -> 0 <= w))]
  where
    pp :: Int -> Int -> [(Maybe Int, (Doc, Int -> Bool))] -> [String]
    pp w p []                               = []
    pp w p ((_, (Nil, _)) : xs)             = pp w p xs
    pp w p ((_, (Text s, _)) : xs)          = s : pp w (p + length s) xs
    pp w p ((Nothing, (Nest j x, _)) : ys)  = pp w p ((Nothing, x) : ys)
    pp w p ((Just i, (Nest j x, _)) : ys)   = pp w p ((Just (i + j), x) : ys)
    pp w p ((i, (Flatten x, _)) : ys)       = pp w p ((Nothing, x) : ys)
    pp w p ((i, (Concat x y, _)) : zs)      = pp w p ((i, x) : (i, y) : zs)
    pp w p ((i, (Choice (x, m) y, _)) : zs) = pp w p ((i, if m (w - p) then (x, m) else y) : zs)
    pp w p ((Nothing, (Line, _)) : xs)      = " " : pp w (p + 1) xs
    pp w p ((Just i, (Line, _)) : xs)       = "\n" : replicate i ' ' : pp w i xs
