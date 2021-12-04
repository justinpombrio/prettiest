{-# LANGUAGE FlexibleInstances #-}

module Swierstra where

-- The implementation from the paper Linear, bounded, functional pretty-printing
-- by Swierstra & Chitil, in the Journal of Functional Programming (functional pearl), 2009.

type Indent = Int -- zero or positive
type Width = Int -- positive
type Layout = String

class Doc d where
  text :: String -> d
  line :: d
  group :: d -> d
  (<>) :: d -> d -> d
  nest :: Indent -> d -> d
  pretty :: Width -> d -> Layout
  nil :: d
  nil = text ""

type Position = Int
type Remaining = Int
type Horizontal = Bool

data Dequeue e = EmptyDQ         -- the empty dequeue
               | e :<| Dequeue e -- prepend an element
               | Dequeue e :|> e -- append an element

type Out = Remaining -> Layout
type OutGroup = Horizontal -> Out -> Out

type Dq = Dequeue (Position, OutGroup)
type TreeCont = Position -> Dq -> Out
type Cont = (Indent, Width) -> TreeCont -> TreeCont

instance Doc Cont where
  text t iw = scan l outText
              where
              l = length t
              outText _ c r = t ++ c (r - l)
  line (i, w) = scan 1 outLine
                where
                outLine True c r = ' ' :                      c (r - 1)
                outLine False c r = '\n' : replicate i ' ' ++ c (w - i)
  (dl <> dr) iw = dl iw . dr iw
  group d iw = \c p dq -> d iw (leave c) p (dq :|> (p, \h c -> c))
  nest j d (i, w) = d (i + j, w)
  pretty w d = d (0, w) (\p dq r -> "") 0 EmptyDQ w
  nil iw = \c -> c

scan :: Width -> OutGroup -> TreeCont -> TreeCont
scan l out c p EmptyDQ = out False (c (p + l) EmptyDQ)
scan l out c p (dq :|> (s, grp)) = prune c (p + l) (dq :|> (s, \h -> grp h . out h))

prune :: TreeCont -> TreeCont
prune c p EmptyDQ r = c p EmptyDQ r
prune c p dq@((s, grp) :<| dq') r | p > s + r = grp False (prune c p dq') r
--                                | True = grp False (prune c p dq') r
prune c p dq r = c p dq r -- Correct? Unclear from paper

leave :: TreeCont -> TreeCont
leave c p EmptyDQ = c p EmptyDQ
leave c p (EmptyDQ :|> (s1, grp1)) = grp1 True (c p EmptyDQ)
leave c p (pp :|> (s2, grp2) :|> (s1, grp1)) =
      c p (pp :|> (s2, \h c -> grp2 h (\r -> grp1 (p <= s1 + r) c r)))
