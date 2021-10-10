import Data.List (replicate)
import Data.Maybe (maybeToList, fromJust)
import Control.Monad (replicateM_)

-- Exponentially slow reference implementation of pretty printing

data Doc =
    Fail
  | Empty
  | Text String
  | NL
  | EOL
  | Indent Width Doc
  | Flat Doc
  | Align Doc
  | Doc :+ Doc
  | Doc :| Doc

type Width = Int

data Layout = Layout [String] Bool
  deriving Show

pretty :: Width -> Doc -> Maybe [String]
pretty w x = case best w (layouts emptyLayout x) of
  Nothing -> Nothing
  Just (Layout lines _) -> Just lines

emptyLayout :: Layout
emptyLayout = Layout [""] False

best :: Width -> [Layout] -> Maybe Layout
best w layouts = pick (map (\lay -> (badness w lay, lay)) layouts)

badness :: Width -> Layout -> Int
badness w (Layout lines _) = overflow w lines
  where
    overflow w [] = 0
    overflow w (line:lines) = (max 0 (length line - w)) + overflow w lines

pick :: [(Int, Layout)] -> Maybe Layout
pick [] = Nothing
pick [(bad, lay)] = Just lay
pick ((bad1, lay1) : (bad2, lay2) : rest) =
  if bad2 < bad1
  then pick ((bad2, lay2) : rest)
  else pick ((bad1, lay1) : rest)

layouts :: Layout -> Doc -> [Layout]
layouts _ Fail = []
layouts lay Empty = [lay]
layouts (Layout lines isFull) (Text t) =
  if isFull && length t > 0
  then []
  else [Layout (init lines ++ [last lines ++ t]) False]
layouts (Layout lines _) NL = [Layout (lines ++ [""]) False]
layouts (Layout lines _) EOL = [Layout lines True]
layouts lay (Indent i x) = map (indent i) (layouts lay x)
layouts lay (Flat x) = filter isFlat (layouts lay x)
  where isFlat (Layout lines _) = length lines == 1
layouts lay@(Layout lines _) (Align x) =
  map (indent (length (last lines))) (layouts lay x)
layouts lay (x :+ y) = [yLay | xLay <- layouts lay x, yLay <- layouts xLay y]
layouts lay (x :| y) = layouts lay x ++ layouts lay y

indent :: Width -> Layout -> Layout
indent i (Layout (line:lines) isFull) =
  Layout (line : (map (addSpaces i) lines)) isFull
  where addSpaces i line = replicate i ' ' ++ line

main =
  let doc = Text "a" :+ Align(Text "b" :+ NL :+ Text "b") :| Text "c"
      lines1 = fromJust $ pretty 80 doc
      lines2 = fromJust $ pretty 1 doc in do
  mapM_ putStrLn lines1
  putStrLn "---"
  mapM_ putStrLn lines2
