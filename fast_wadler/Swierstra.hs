{-# LANGUAGE FlexibleInstances #-}

module Swierstra where

import qualified Dequeue as DQ
import Dequeue (BankersDequeue)

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

type Dequeue e = BankersDequeue e

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
  group d iw = \c p dq -> d iw (leave c) p (dq `DQ.pushBack` (p, \h c -> c))
  nest j d (i, w) = d (i + j, w)
  pretty w d = d (0, w) (\p dq r -> "") 0 DQ.empty w
  nil iw = \c -> c

scan :: Width -> OutGroup -> TreeCont -> TreeCont
scan l out c p dq =
  case DQ.popBack dq of
    Nothing -> out False (c (p + l) DQ.empty)
    Just ((s, grp), dq) ->
      prune c (p + l) (dq `DQ.pushBack` (s, \h -> grp h . out h))

prune :: TreeCont -> TreeCont
prune c p dq r =
  case DQ.popFront dq of
    Nothing -> c p DQ.empty r
    Just ((s, grp), dq') | p > s + r -> grp False (prune c p dq') r
                         | True      -> c p dq r

leave :: TreeCont -> TreeCont
leave c p dq =
  case DQ.popBack dq of
    Nothing -> c p DQ.empty
    Just ((s1, grp1), dq') ->
      case DQ.popBack dq' of
        Nothing -> grp1 True (c p DQ.empty)
        Just ((s2, grp2), pp) ->
          c p (pp `DQ.pushBack`
               (s2, \h c -> grp2 h (\r -> grp1 (p <= s1 + r) c r)))
