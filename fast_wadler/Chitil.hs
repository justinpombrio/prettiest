module Chitil (Doc(Doc), pretty, text, line, (<+>), group, nestMargin, nestCol) where

import Prelude hiding (head,last,init,tail,reverse)
import qualified Data.List as List

-- IMPLEMENTATION OF THE DOCUMENT TYPE

text :: String -> Doc
text s = Doc (Text s)

line :: Doc
line = Doc (Line)

(<+>) :: Doc -> Doc -> Doc
Doc l1 <+> Doc l2 = Doc (l1 . l2)

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

data Tokens = Empty | Text String Tokens | Line Tokens
            | Open Tokens | Close Tokens
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

-- THE LAZY DEQUEUE PRETTY PRINTING LIBRARY

pretty :: Int -> Doc -> String
pretty w doc = fst (inter (doc2Tokens doc) 0 w [0] empty)
  where
  inter :: Tokens -> Int -> Int -> [Int] -> Seq Int -> (String,Seq Bool)
  inter Empty _ _ _ _ = ("",empty)
  inter (Text s ts) p r ms es = (s ++ o,hs)
    where
    (es',hs) = prune p' es hs'
    (o,hs') = inter ts p' (r-l) ms es'
    l = length s
    p' = p+l
  inter (Line ts) p r ms es =
    (if h then ' ' : o else '\n' : rep m ' ' o,hs)
    where
    (es',hs) = prune p' es hs'
    (o,hs') = inter ts p' (if h then r-1 else w-m) ms es'
    h = not (isEmpty es') && head hs'
    p' = p+1
    m = List.head ms
  inter (Open ts) p r ms es = (o,hs)
    where
    (es',hs) = consTail (p+r) es hs'
    (o,hs') = inter ts p r ms es'
  inter (Close ts) p r ms es = (o,hs)
    where
    (es',hs) = if isEmpty es then (es,hs') else tailCons es (p <= head es) hs'
    (o,hs') = inter ts p r ms es'
  inter (OpenNest f ts) p r ms es =
    inter ts p r ((f (List.head ms) (w-r)) : ms) es
  inter (CloseNest ts) p r ms es =
    inter ts p r (List.tail ms) es

prune :: Int -> Seq Int -> Seq Bool -> (Seq Int,Seq Bool)
prune p es1 hs3 =
  if isEmpty es1 || p <= last es1 then (es1,hs3) else (es3,hs1)
  where
  (es2,hs1) = initSnoc es1 False hs2
  (es3,hs2) = prune p es2 hs3

-- variant of `replicate': rep n x rs = replicate n x ++ rs
rep :: Int -> a -> [a] -> [a]
rep n x rs = if n <= 0 then rs else x : rep (n-1) x rs

-- IMPLEMENTATION OF EFFICIENT LAZY DEQUEUES

copyListStructure :: [a] -> [b] -> [b]
copyListStructure [] _ = []
copyListStructure (_:xs) zs = y: copyListStructure xs ys
  where
  (y:ys) = zs

data Seq a = S !Int [a] !Int [a]

empty :: Seq a
empty = S 0 [] 0 []

isEmpty :: Seq a -> Bool
isEmpty (S lenf _ lenr _) = (lenf + lenr == 0)

head (S lenf f lenr r) = List.head (if lenf==0 then r else f)
last (S lenf f lenr r) = List.head (if lenr==0 then f else r)

consTail :: a -> Seq a -> Seq b -> (Seq a,Seq b)
consTail x (S lenf f lenr r) sq =
  (sq',S lenf (List.tail f') lenr r')
  where
  (sq',f',r') = check (lenf+1) (x:f) lenr r sq
  -- precondition: sq and sq' have same structure

tailCons :: Seq a -> b -> Seq b -> (Seq a,Seq b)
tailCons (S _ [] _ _) x _ = (empty,S 0 [] 1 [x])
tailCons (S lenf f lenr r) x sq =
  (reverse sq',S lenf (x:f') lenr r')
  where
  (sq',r',f') = check lenr r (lenf-1) (List.tail f) (reverse sq)
  -- precondition: sq and sq' have same structure

initSnoc :: Seq a -> b -> Seq b -> (Seq a,Seq b)
initSnoc (S _ _ _ []) x _ = (empty,S 1 [x] 0 [])
initSnoc (S lenf f lenr r) x sq =
  (sq',S lenf f' lenr (x:r'))
  where
  (sq',f',r') = check lenf f (lenr-1) (List.tail r) sq
  -- precondition: sq and sq' have same structure

reverse :: Seq a -> Seq a
reverse (S lenf f lenr r) = S lenr r lenf f

-- Keep lists in balance: rebalance if front list too long
check :: Int -> [a] -> Int -> [a] -> Seq b -> (Seq a,[b],[b])
check lenf f lenr r sq = if lenf <= 3 * lenr + 1
                         then (S lenf f lenr r,f2,r2)
                         else (S lenf' f' lenr' r',f2',r2')
  where
  S _ f2 _ r2 = sq
  len = lenf + lenr
  lenf' = len `div` 2
  lenr' = len - lenf'
  (f',rf') = splitAt lenf' f
  r' = r ++ List.reverse rf'
  lf2 = copyListStructure f' f2
  lr2 = copyListStructure r' r2
  (r2',rf2') = splitAt lenr lr2
  f2' = lf2 ++ List.reverse rf2'
