{-# LANGUAGE ScopedTypeVariables #-}

-- Incremental pretty-printing without laziness

module Kiselyov (Doc (Text, Line, Group, (:+:)), mainD, mainS) where

{-
 The manuscript
    Laziness vs Continuations: Prettyprinting without Backtracking
    Amr Sabry and Simon Peyton-Jones
  (hereafter SPJ) describes the online incremental pretty-printing
  algorithm implemented with full co-routines (full continuations).

 This code shows that a low-powered generators (which were introduced in
 CLU and were present in early version of Python 2) are sufficient.
 We mean the generators in which yield reports the value `upstairs'
 without receiving anything back. The information flows strictly up
 the stack. These low-powered generators (unlike full continuations)
 can be easily implemented, on a single linear stack without copying.

 In our approach, the entire formatting is a composition of two stream
 transducers, operating on the stream of nodes of the source document.
 The computation is efficient since it is clear by inspection that
 width computations are repeated (such repetition was the cause of
 exponential or O(n*w) complexity in naive solutions to the problem).
 The traces below demonstrate that the look-ahead is limited,
 by PageWidth.

 Doaitse Swierstra has also developed an online linear pretty-printing
 algorithm:
  http://www.cs.uu.nl/research/techreps/repo/CS-2004/2004-025a.pdf
 which crucially relies on laziness.
 
 The main feature of our approach is that it is not lazy. It can be
 implemented in a strict language. Mainly, it is compatible with IO and
 effectful computations: we can read the source document from a file,
 and format is as we read.

-}

import Control.Monad.Trans

import Data.Sequence (ViewL(..),ViewR(..),(<|),(|>)) -- Queue
import qualified Data.Sequence as S -- Queue    
import Data.List (intersperse)

import GenT -- simple generators

-- For benchmarking
import Data.Monoid
import Control.Monad.Writer
import System.Environment (getArgs)
import System.Mem
import GHC.Stats

-- ------------------------------------------------------------------------
-- Problem Specification, from the SPJ paper (Sec 2)

-- A document is represented as a tree
data Doc = Text String
  | Line
  | Doc :+: Doc
  | Group Doc
  deriving Show

-- A sample document
doc1 = 
 Group (Text "A" :+:
  (Line :+:
   Group (Text "B" :+: 
           (Line :+:
             Text "C"))))

-- It seems long lines are not split, and line
-- breaks can only occur at `Line'

-- The reference implementation
-- See Sec 2 of the paper and also Chitil (ref 3)

type Fit = Bool
type WidthLeft = Int
type Width     = Int
type PageWidth = Int

pretty1 :: PageWidth -> Doc -> String
pretty1 w d = fst $ format False w d
 where
 format :: Fit -> WidthLeft -> Doc -> (String, WidthLeft)
 format f r (Text z) = (z, r - length z)
 format True  r Line = (" ", r-1)
 format False r Line = ("\n",w)
 format f r (d1 :+: d2) = (s1++s2, r2)
  where (s1,r1) = format f r d1
        (s2,r2) = format f r1 d2
 format f r (Group d) = format (f || width d <= r) r d

 width :: Doc -> Width
 width (Text z)    = length z
 width Line        = 1
 width (d1 :+: d2) = width d1 + width d2
 width (Group d)   = width d

-- Sample formatting
t01 = pretty1 6 doc1
-- "A B C"
t02 = pretty1 5 doc1
-- "A B C"
t03 = pretty1 4 doc1
-- "A\nB C"
t04 = pretty1 3 doc1
-- "A\nB C"
t05 = pretty1 2 doc1
-- "A\nB\nC"
t06 = pretty1 1 doc1
-- "A\nB\nC"

{-

The naive algorithm is exponential because the width of a deeply
nested group is repeatedly re-computed.

Solution: compute the width of all groups beforehand.
Let's traverse the whole Doc tree and annotate each node with its
width. That is the standard post-order traversal, which is linear
in the size of the tree. Alas, the intermediate tree needs
as much space as the original tree. Since we have to traverse
all children of the root node (that is, all the tree) to
compute the width of the root node, we really have to build
the whole tree first before we start formatting.

We thus recover the standard lazy algorithm (Sec 3.1), but
without laziness. As the lazy algorithm, ours works in linear
time (two traversals of the tree) but needs extra space equal
to the size of the tree. But we didn't need laziness, we didn't
need to build extra closures, and we didn't need to play the
tricks with tying the knot.

Because of that, we can do IO.
-}

-- Here we demonstrate the building of the intermediary tree,
-- DocW, in which each group is annotated with its width.

data DocW = TextW String
  | LineW
  | JoinW  DocW DocW
  | GroupW Width DocW
  deriving Show


buildDocW :: Doc -> DocW
buildDocW d = fst $ go 0 d
 where
 -- We thread the running width throughout
 go :: Width -> Doc -> (DocW, Width)
 go p (Text z) = (TextW z, p + length z)
 go p Line     = (LineW, p + 1)
 go p (d1 :+: d2) = let (d1',p1) = go p  d1
                        (d2',p2) = go p1 d2
                    in (JoinW d1' d2', p2)
 go p (Group d) = let (d',p') = go p d
                  in (GroupW (p'-p) d',p')

docw1 = buildDocW doc1

{-
 GroupW 5 (JoinW (TextW "A") 
    (JoinW LineW 
      (GroupW 3 (JoinW (TextW "B") (JoinW LineW (TextW "C"))))))
-}


{-
We come to the second optimization: If the width of a tree node
exceeds the PageWidth, we don't need to know by how much.
We merely need to compute the width up to PageWidth (which is the
constant, independent of the Doc size). Thus, we can assign the
width to a node before finishing traversal of its children.
Thus we can do with less auxiliary space.

To realize this savings and make the algorithm incremental,
we use the standard trick. We traverse the tree and yield
the encountered nodes, converting a tree to a stream.
-}

-- First, we re-write the above algorithm to be incremental, to produce
-- the stream.
-- One may imagine that the document is written on disk, as a sort
-- of XML. The procedure genB below then reads the nodes
-- of the document one by one.

-- This is the data type for the elements of the document stream
-- The data type is parametrized by annotations; GBeg may have
-- its own annotation.
data E ab a = TE a String
  | LE a
  | GBeg ab
  | GEnd a
   deriving Show

type StreamB = E () () -- bare stream, no annotations

-- The type shows that we emit StreamB elements as we go
-- We use any base monad, including IO.
genB :: Monad m => Doc -> GenT StreamB m ()
genB (Text "")   = return ()
genB (Text z)    = yield (TE () z)
genB Line        = yield (LE ())
genB (d1 :+: d2) = genB d1 >> genB d2
genB (Group d0) | Just d <- norm d0 = do
  yield (GBeg ())
  genB d
  yield (GEnd ())
 where
 norm (Group d)       = norm d
 norm (Text "")       = Nothing
 norm (Text "" :+: d) = norm d
 norm d               = Just d
genB _ = return ()

-- Let's check the stream for the sample document doc1
streamB :: IO ()
streamB = runGenT (genB doc1)
  (\i -> putStrLn $ "Generated: " ++ show i)

-- The function genB also performs normalization of the
-- document:
--   every group is strictly wider than any of its children groups
--    (thus eliminating Group $ Group ...)
--   any group is at least one-character wide (eliminating Group (Text ""))

-- Analysis: latency is unit, total time to generate the whole stream
-- is linear in the size of the document, space is O(depth) since
-- genB is non-tail-recursive.


-- To easily compute the width of any group without repetition, we
-- precompute the ``rolling width'', or the horizontal position
-- assuming the whole document is formatted in one line.
-- TE, LE, and GEnd elements are annotated with the horizontal position
-- after the element has been formatted.
-- GBeg is annotated with the horizontal position before the group
-- is formatted.

-- We stress the modularity of adding the annotation.

type HP  = Int -- Horizontal position, after formatting
type HPB = Int -- position before the formatting

type StreamHPB = E HPB HP
trHPB :: Monad m => GenT StreamB (StateT HP (GenT StreamHPB m)) ()
  -> GenT StreamHPB m ()
trHPB = foldG_ go 0
 where
 go :: Monad m => HP -> StreamB -> GenT StreamHPB m HP
 go p (TE _ z) = let p' = p + length z in
   yield (TE p' z) >> return p'
 go p (LE _)   = let p' = p + 1 in
                 yield (LE p')   >> return p'
 go p (GBeg _) = yield (GBeg p)  >> return p
 go p (GEnd _) = yield (GEnd p)  >> return p


-- Let's check the stream for the sample document doc1
streamHPB :: IO ()
streamHPB = runGenT (trHPB . genB $ doc1)
  (\i -> putStrLn $ "Generated: " ++ show i)

{-
Generated: GBeg 0
Generated: TE 1 "A"
Generated: LE 2
Generated: GBeg 2
Generated: TE 3 "B"
Generated: LE 4
Generated: TE 5 "C"
Generated: GEnd 5
Generated: GEnd 5
-}

-- Complexity analysis: latency is unit, overall time complexity 
-- is linear in the length of the stream. Space: constant.

-- Changing the stream so that GBeg has HP of the end of the group
-- rather than of the beginning.

type StreamHPA = E HP HP

-- Look-ahead buffer
-- The buffer is a list with an element BufHPA for each nested group
type Buffer m = [Buf StreamHPA m]
type Buf e m = GenT e m ()

-- Buf, the look-ahead buffer proper, should permit the following operations
--   -- empty
--   -- append one element in constant time
--   -- prepend one element in constant time
--   -- concatenate two buffers in constant time
--      (could be relaxed to, say log time)
--   -- fold over the buffer and emit all accumulated elements, in linear time

buf_empty :: Monad m => Buf e m
buf_empty = return ()

infixl 5 ||>
infixr 5 <||

(||>) :: Monad m => Buf e m -> e -> Buf e m
b ||> e = b >> yield e

(<||) :: Monad m => e -> Buf e m -> Buf e m
e <|| b = yield e >> b

buf_ccat :: Monad m => Buf e m -> Buf e m -> Buf e m
buf_ccat b1 b2 = b1 >> b2

buf_emit :: Monad m => Buf e m -> GenT e m ()
buf_emit b = b

trHPA :: Monad m => GenT StreamHPB (StateT (Buffer m) (GenT StreamHPA m)) () 
      -> GenT StreamHPA m ()
trHPA = foldG_ go []
 where
 -- switch-on the look-ahead
 go q (GBeg _)       = return (buf_empty:q)
 -- GEnd flushes the top layer
 go (b:q) e@(GEnd p) = pop q (GBeg p <|| (b ||> e))
 -- if no look-ahead, emit immediately
 go [] e = yield e >> return []
 -- otherwise, delay
 go (b:q) e = return ((b ||> e):q)

 pop [] b     = buf_emit b >> return []
 pop (b':q) b = return ((buf_ccat b' b):q)

streamHPA :: IO ()
streamHPA = runGenT (trHPA . trHPB . genB $ doc1)
  (\i -> putStrLn $ "Generated: " ++ show i)

{-
Generated: GBeg 5
Generated: TE 1 "A"
Generated: LE 2
Generated: GBeg 5
Generated: TE 3 "B"
Generated: LE 4
Generated: TE 5 "C"
Generated: GEnd 5
Generated: GEnd 5
-}

-- Analysis: latency: n (have to wait till GEnd)
-- Extra space: n (the look-ahead buffer)
-- Total time: for each read element, give credit 2.
-- GEnd uses credits for all the elements in the group it terminates.
-- So, total time is 2n
-- Thus to find out the width of a group, one has to wait till
-- the corresponding GEnd element.

-- Pruned HP

data HPP = Small HP | TooFar deriving Show

type StreamHPP = E HPP HP


-- HPL is the limit (starting position for the group + PageWidth)
type HPL = Int

-- Look-ahead buffer
-- It is like Buffer before; we need a queue rather than a simple list
-- Also, we separately keep track of the limit position for each
-- nested group, and for the outer group.
type BufferP m = (HPL, S.Seq (HPL, Buf StreamHPP m))
bufferP_empty = (0,S.empty)

-- trHPP :: (Monad m, MonadIO m) => 
trHPP :: (Monad m) => 
      PageWidth
      -> GenT StreamHPB (StateT (BufferP m) (GenT StreamHPP m)) () 
      -> GenT StreamHPP m ()
trHPP w = foldG_ go' bufferP_empty
 where
 go' s e = do
  -- liftIO . putStrLn $ "trHPP: read: " ++ show e
  go s e

 go b@(_,q) (TE p z) | S.null q = yield (TE p z) >> return b
 go b@(_,q) (LE p)   | S.null q = yield (LE p)   >> return b
 go b@(_,q) (GEnd p) | S.null q = yield (GEnd p) >> return b

 -- GBeg switches on the look-ahead
 go (_,q) (GBeg p) | S.null q = 
   return (p+w,S.singleton (p+w,buf_empty))
 go (p0,q) (GBeg p) = 
   check (p0,q |> (p+w,buf_empty)) p
 go (p0,q) (GEnd p) | q' :> (_,b) <- S.viewr q =
   pop p0 q' (GBeg (Small p) <|| (b ||> (GEnd p)))

 go (p0,q) (TE p z) = check (p0,push (TE p z) q) p
 go (p0,q) (LE p)   = check (p0,push (LE p) q) p

 push e q | q' :> (p,b) <- S.viewr q = q' |> (p,b ||> e)
 
 pop p0 q b | S.null q = buf_emit b >> return bufferP_empty
 pop p0 q b | q' :> (p,b') <- S.viewr q = 
  return (p0, q' |> (p,buf_ccat b' b))
 
 -- Check if the outer group is already too wide
 check (p0,q) p | p <= p0 && S.length q <= w = return (p0,q)
 check (_,q) p | (_,b) :< q' <- S.viewl q = 
  buf_emit (GBeg TooFar <|| b) >> check' q' p

 check' q p | (p',_) :< _ <- S.viewl q = check (p',q) p
            | otherwise                = return bufferP_empty

streamHPP :: PageWidth -> IO ()
streamHPP w = runGenT (trHPP w . trHPB . genB $ doc1)
  (\i -> putStrLn $ "Generated: " ++ show i)

{-
Here is a different edge case

	Group (Group (Group ... :+: Group ...) :+:
	       Group (Group ... :+: Group ...) )

An unwary algorithm has to traverse to the leafs before it decides
if a group fits; the leaves may be arbitrarily deep. The solution is
to notice that if the document is normalized (and each group is at
least one character wide), we can stop the traversal after w groups
(even if we found no Text element). 
-}

-- The trace below clearly shows the look-ahead
-- and the fact that it is limited by the page width.
-- For page width < 5, we start emitting StreamHPP elements
-- before we read all of StreamHPB.
{-
*PPYield> streamHPP 5
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
trHPP: read: TE 5 "C"
trHPP: read: GEnd 5
trHPP: read: GEnd 5
Generated: GBeg (Small 5)
Generated: TE 1 "A"
Generated: LE 2
Generated: GBeg (Small 5)
Generated: TE 3 "B"
Generated: LE 4
Generated: TE 5 "C"
Generated: GEnd 5
Generated: GEnd 5

*PPYield> streamHPP 4
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
trHPP: read: TE 5 "C"
Generated: GBeg TooFar
Generated: TE 1 "A"
Generated: LE 2
trHPP: read: GEnd 5
Generated: GBeg (Small 5)
Generated: TE 3 "B"
Generated: LE 4
Generated: TE 5 "C"
Generated: GEnd 5
trHPP: read: GEnd 5
Generated: GEnd 5

*PPYield> streamHPP 3
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
Generated: GBeg TooFar
Generated: TE 1 "A"
Generated: LE 2
trHPP: read: TE 5 "C"
trHPP: read: GEnd 5
Generated: GBeg (Small 5)
Generated: TE 3 "B"
Generated: LE 4
Generated: TE 5 "C"
Generated: GEnd 5
trHPP: read: GEnd 5
Generated: GEnd 5

*PPYield> streamHPP 2
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
Generated: GBeg TooFar
Generated: TE 1 "A"
Generated: LE 2
trHPP: read: LE 4
trHPP: read: TE 5 "C"
Generated: GBeg TooFar
Generated: TE 3 "B"
Generated: LE 4
Generated: TE 5 "C"
trHPP: read: GEnd 5
Generated: GEnd 5
trHPP: read: GEnd 5
Generated: GEnd 5

*PPYield> streamHPP 1
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
Generated: GBeg TooFar
Generated: TE 1 "A"
Generated: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
Generated: GBeg TooFar
Generated: TE 3 "B"
Generated: LE 4
trHPP: read: TE 5 "C"
Generated: TE 5 "C"
trHPP: read: GEnd 5
Generated: GEnd 5
trHPP: read: GEnd 5
Generated: GEnd 5
-}

-- Analysis: quite like trHPA. However, we keep in mind that the buffer
-- length is bound by w. Therefore, latency and extra space are
-- all bounded by w rather than n.


-- Formatting
-- Producing the stream of Strings

-- The final step is the formatting: transforming the pruned
-- StreamHPP to a stream of Strings.

-- Indication if Lines within a group should be formatted as spaces.
-- This is the stack reflecting Fit for the nested groups
type Fits = [Bool]

-- Again we use the finite state transducer foldG, whose state
-- is (Fits,HPL). The second component is the HP at the _end_ of
-- the current line (any HP past that is over the page width).
trFormat0 :: Monad m =>
   PageWidth 
   -> GenT StreamHPP (StateT (Fits, HPL) (GenT String m)) () 
   -> GenT String m ()
trFormat0 w = foldG_ go ([False],w)
 where
 go (f,l) (TE _ z) = yield z >> return (f,l)
 go (f@(True:_),l)  (LE _) = yield " "  >> return (f,l)
 go (f@(False:_),l) (LE p) = yield "\n" >> return (f,p+w)
 go (f,l) (GBeg TooFar)    = return (False:f,l)
 go (f,l) (GBeg (Small p)) = return ((p <= l):f,l)
 go (_:f,l) (GEnd _)       = return (f,l)

-- Analysis: unit latency (we immediately emit the received data)
-- Overall processing time is linear in the size of the stream.
-- Alas, we need extra space for the stack Fits. We will
-- eliminate that below.

streamF0 :: PageWidth -> IO ()
streamF0 w = runGenT (trFormat0 w . trHPP w . trHPB . genB $ doc1)
  (\i -> putStrLn $ "Generated: " ++ show i)

-- Formatting of the sample document
-- Again, for small PageWidth, we emit the formatted strings before
-- we have read the whole input stream. Our algorithm is on-line.
{-
streamF0 5
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
trHPP: read: TE 5 "C"
trHPP: read: GEnd 5
trHPP: read: GEnd 5
Generated: "A"
Generated: " "
Generated: "B"
Generated: " "
Generated: "C"

streamF0 4
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
trHPP: read: TE 5 "C"
Generated: "A"
Generated: "\n"
trHPP: read: GEnd 5
Generated: "B"
Generated: " "
Generated: "C"
trHPP: read: GEnd 5

streamF0 3
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
Generated: "A"
Generated: "\n"
trHPP: read: TE 5 "C"
trHPP: read: GEnd 5
Generated: "B"
Generated: " "
Generated: "C"
trHPP: read: GEnd 5

streamF0 2
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
Generated: "A"
Generated: "\n"
trHPP: read: LE 4
trHPP: read: TE 5 "C"
Generated: "B"
Generated: "\n"
Generated: "C"
trHPP: read: GEnd 5
trHPP: read: GEnd 5

streamF0 1
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
Generated: "A"
Generated: "\n"
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
Generated: "B"
Generated: "\n"
trHPP: read: TE 5 "C"
Generated: "C"
trHPP: read: GEnd 5
trHPP: read: GEnd 5

-}

-- Better formatting

-- The stack Fits has a particular property, following
-- from the observation that if a group fits, all of its children
-- groups fit as well. Therefore, if an element of the stack is False,
-- the elements below must all be False. Conversely, if an element
-- is True, the elements above it, through the top of the stack, must
-- all be True. Thus the whole stack is adequately represented
-- by a single number, the number of True elements at the top.

type FitI = Int

trFormat :: Monad m =>
   PageWidth 
   -> GenT StreamHPP (StateT (FitI, HPL) (GenT String m)) () 
   -> GenT String m ()
trFormat w = foldG_ go (0,w)
 where
 go (f,l) (TE _ z) = yield z    >> return (f,l)
 go (0,l) (LE p)   = yield "\n" >> return (0,p+w)
 go (f,l) (LE _)   = yield " "  >> return (f,l)
 go (0,l) (GBeg TooFar)    = return (0,l)
 go (0,l) (GBeg (Small p)) = return (if p <= l then 1 else 0,l)
 go (f,l) (GBeg _)         = return (f+1,l)
 go (0,l) (GEnd _)         = return (0,l)
 go (f,l) (GEnd _)         = return (f-1,l)

streamF :: PageWidth -> IO ()
streamF w = runGenT (trFormat w . trHPP w . trHPB . genB $ doc1)
  (\i -> putStrLn $ "Generated: " ++ show i)
{-
streamF 5
streamF 4
streamF 3
streamF 2

streamF 1
trHPP: read: GBeg 0
trHPP: read: TE 1 "A"
trHPP: read: LE 2
Generated: "A"
Generated: "\n"
trHPP: read: GBeg 2
trHPP: read: TE 3 "B"
trHPP: read: LE 4
Generated: "B"
Generated: "\n"
trHPP: read: TE 5 "C"
Generated: "C"
trHPP: read: GEnd 5
trHPP: read: GEnd 5
-}

-- Final pretty-printer

pp :: Monad m => PageWidth -> Doc -> GenT String m ()
pp w = trFormat w . trHPP w . trHPB . genB


{-
-- Old version

-- Elements of the stream
-- Each element is annotated with the rolling width
-- (the width after the element has been emitted)
data DocE1 = TE1 Width String
   | LE1 Width
   | GBeg1 GID Width
   | GEnd1 GID Width
 deriving Show

type GID = Int 				-- Group identifier

-- We transform DocE1 to a different stream.
-- It is the beginning of the group that contains the width
-- of the group.
-- The crucial part is that we compute the width up to
-- PageWidth; anything wider is TooWide.

data LWidth = Small Width | TooWide deriving Show

data DocE2 = TE2 String
   | LE2
   | GBeg2 GID LWidth
   | GEnd2 GID
 deriving Show

-- We use the queue and the IntMap
-- Operations in IntMap are essentially constant-time

-- We use a queue for accumulating look-ahead. 
-- The queue is bounded because of PageWidth!
-- Invariant: the left end of the non-empty queue is GBeg1
type Queue = S.Seq DocE1

-- Binding for `logic variables'
-- Each group as an associated logic variable
-- Status of the logic variable
-- If a `logic variable' contains LV_abs, it will
-- be `rebound' to LV_rel. If the logic variable
-- is already bound to LV_rel, it will never be rebound.
-- Thus, width computations are done only once and never
-- repeated.
-- in imperative language, we can implement LVar using simple references
-- cells. 
-- We stress that complexity of pointer dereference and lookup in IntMap are the
-- same: if we take pointer dereference to be constant, under the same
-- assumptions IntMap lookup is constant. If the size of the memory is
-- not bounded, then both pointer dereference and Map lookup take
-- O(log N) time where N is the size of the heap.

data LVar = LV_abs Width -- absolute position
	  | LV_rel Width -- width of the group
type Subst = M.IntMap LVar

-- As the type shows, we transform the stream of DocE1 to the stream
-- of DocE2, using the finite state transducer foldG whose state
-- is (Queue,Subst)
transp_stream2 :: forall m. 
  (Monad m, MonadIO m) => 
   PageWidth -> 
   GenT DocE1 (StateT (Queue, Subst) (GenT DocE2 m)) () ->
   GenT DocE2 m ()
transp_stream2 w gen = foldG go (S.empty,M.empty) gen
 where
 go ::  (Queue,Subst) -> DocE1 -> GenT DocE2 m (Queue,Subst)
 go (q,subst) e = do
  liftIO . putStrLn $ "transp: read: " ++ show e
  check q subst e
 
 -- no look-ahead: emit the elements as we got them
 check q subst (TE1 _ z)   | S.null q = yield (TE2 z)   >> return (q,subst)
 check q subst (LE1 _)     | S.null q = yield LE2       >> return (q,subst)
 check q subst (GEnd1 g _) | S.null q = yield (GEnd2 g) >> return (q,subst)

 -- GBeg1 switches on the look-ahead
 -- We enter a fresh logic variable for the group
 check q subst e@(GBeg1 g p) = flush p (q |> e) (M.insert g (LV_abs p) subst)
 check q subst e@(GEnd1 g p) = 
  flush' p (M.adjust adj g subst) (S.viewl (q |> e))
   where adj (LV_abs p') = LV_rel (p - p')
 check q subst e@(TE1 p z) = flush p (q |> e) subst
 check q subst e@(LE1 p)   = flush p (q |> e) subst

 -- check to see if the group whose GBeg is at the left end of
 -- the queue has become too wide. If so, we can emit GBeg2
 -- and we flush all delayed elements until the first GBeg whose
 -- size is not determined yet.
 -- Since we normalize the stream (see norm above), if the number
 -- of open groups exceeds w, the topmost group is too wide.
 -- If subst[g] = LV_abs _, then the width of the group g is not yet 
 -- determined.
 -- BTW, viewl is the O(1) operation.
 flush p q subst | GBeg1 g p' :< q' <- S.viewl q =
   if p - p' > w then 
        yield (GBeg2 g TooWide) >> flush' p subst (S.viewl q')
      else return (q,subst)

 -- now really do the flushing
 -- As we emit GEnd2 g, we may remove
 -- the binding for g from subst; we won't need it again.
 flush' p subst EmptyL          = return (S.empty, subst)
 flush' p subst (TE1 _ z :< q') = 
    yield (TE2 z) >> flush' p subst (S.viewl q')
 flush' p subst (LE1 _  :< q') =
    yield LE2 >> flush' p subst (S.viewl q')
 flush' p subst (GEnd1 g _  :< q') =
    yield (GEnd2 g) >> flush' p (M.delete g subst) (S.viewl q')
 flush' p subst (GBeg1 g _  :< q') | 
       (Just (LV_rel pd)) <- M.lookup g subst =
    yield (GBeg2 g (Small pd)) >> flush' p subst (S.viewl q')
 flush' p subst (a :< q) = flush p (a <| q) subst


-- Check the sample stream
emit_stream2_r :: PageWidth -> IO ()
emit_stream2_r w = runGenT (transp_stream2 w . emit_stream1 $ doc1)
   (\i -> putStrLn $ "Generated: " ++ show i)
-}


-- ------------------------------------------------------------------------
-- Pretty-printing of a tree example, from Swierstra-linear

data Tree = Tree String [Tree ]
tree1 = Tree "aaa" [Tree "bbbb" [Tree "ccc" [ ]
                               , Tree "dd" [ ]
                                ]
                , Tree "eee" [ ]
                , Tree "ffff" [Tree "gg" [ ]
                               , Tree "hhh" [ ]
                               , Tree "ii" [ ]
                                ]
                ]


show_tree  (Tree s ts) = Group
                        ( Text s :+:    -- nest (length s)
                          (show_kids ts)
                        )
 where
 show_kids []   = Text ""
 show_kids ts   = Text "[" :+: show_forest ts :+: Text "]"
 show_forest ts = foldr (:+:) (Text "")
                    (intersperse (Text "," :+: Line) (Prelude.map show_tree ts))

{-
15 and 20
aaa[bbbb[ccc, aaa[bbbb[ccc, dd],
         dd],     eee,
    eee,          ffff[gg,
    ffff[gg,           hhh,
         hhh,          ii]]
         ii]]
-}

doct = show_tree tree1

treeB :: IO ()
treeB = runGenT (genB doct)
  (\i -> putStrLn $ "Generated: " ++ show i)

treeHPP :: PageWidth -> IO ()
treeHPP w = runGenT (pp w doct)
  (\i -> putStrLn $ "Generated: " ++ show i)
{-
treeHPP 15
treeHPP 20
-}

-- ------------------------------------------------------------------------
-- Benchmarking

-- build a `realistic' benchmark document tree of the depth d

doc_bench :: Int -> Doc
doc_bench d = go 1 d
 where 
 go l 0 = Group (Text (show l))
 go l d = Group (Text (show l ++ "[") :+: 
   go (2*l) (d-1) :+: Line :+:
   go (2*l+1) (d-1) :+: Text "]")

{-
doc_bench 2
Group ((((Text "1[" :+: Group ((((Text "2[" :+: Group (Text "4")) :+: Line) :+: Group (Text "5")) :+: Text "]")) :+: Line) :+: Group ((((Text "3[" :+: Group (Text "6")) :+: Line) :+: Group (Text "7")) :+: Text "]")) :+: Text "]")

-}

-- Size the content of the tree
-- (doing so also forces the tree)
doc_count :: Doc -> Int
doc_count (Text s)    = length s
doc_count Line        = 1
doc_count (d1 :+: d2) = doc_count d1 + doc_count d2
doc_count (Group d)   = doc_count d

-- doc_count $ doc_bench 2
-- 16

-- Generate the stream that corresponds to doc_bench, without
-- constructing the doc itself
-- The document tree is the main consumer of space
-- The function is not tail recursive, and so consumes space 
-- proportional to the depth of the tree d.
stream_bench :: Monad m => Int -> GenT StreamB m ()
stream_bench d = go 1 d
 where
 go l 0 = do
  yield (GBeg ())
  yield (TE () (show l))
  yield (GEnd ())
 go l d = do
  yield (GBeg ())
  yield (TE () (show l ++ "["))
  go (2*l) (d-1)
  yield (LE ())
  go (2*l+1) (d-1)
  yield (TE () "]")
  yield (GEnd ())
  
-- Let's check the stream for the sample document doc1
{-
 runGenT (stream_bench 2) (\i -> putStrLn $ "Generated: " ++ show i)
-}



benchF :: Int -> PageWidth -> IO ()
benchF d w = runGenT (pp w $ doc_bench d)
  (\i -> putStrLn $ "Generated: " ++ show i)
{-
benchF 2 7
Generated: "1["
Generated: "2["
Generated: "4"
Generated: "\n"
Generated: "5"
Generated: "]"
Generated: "\n"
Generated: "3["
Generated: "6"
Generated: " "
Generated: "7"
Generated: "]"
Generated: "]"

benchF 2 2
-}


-- Using the already generated document stream 
pp' w = trFormat w . trHPP w . trHPB

benchF' :: Int -> PageWidth -> IO ()
benchF' d w = runGenT (pp' w $ stream_bench d)
  (\i -> putStrLn $ "Generated: " ++ show i)

{-
benchF' 2 7
Generated: "1["
Generated: "2["
Generated: "4"
Generated: "\n"
Generated: "5"
Generated: "]"
Generated: "\n"
Generated: "3["
Generated: "6"
Generated: " "
Generated: "7"
Generated: "]"
Generated: "]"
-}


-- For benchmarking purposes, instead of writing the formatted output,
-- we count the total size and the number of lines

-- Initially I didn't have strict fields below, and the memory
-- usage was linear! Laziness always gets you!
data Counts = Counts !Int !Int deriving Show

instance Monoid Counts where
    mempty = Counts 0 0
    mappend (Counts c1 l1) (Counts c2 l2) = Counts (c1+c2) (l1+l2)

fmt_count :: Doc -> PageWidth -> Counts
fmt_count d w = execWriter $ runGenT 
     (pp w d)
     (\i -> tell $ Counts (length i) (if i == "\n" then 1 else 0))

-- Building the tree first
benchD :: Int -> PageWidth -> Counts
benchD d w = fmt_count (doc_bench d) w


{-
benchD 2 7
-- Counts 16 2

benchD 2 2
-- Counts 16 3

benchD 15 10
-- Counts 414870 32767

benchmarking: benchC 10 50
-}

-- Working directly from the stream, avoiding constructing the original
-- document, which takes a lot of memory.
-- Use benchS to show formatting proceeds in constant memory.
benchS :: Int -> PageWidth -> Counts
benchS d w = execWriter $ runGenT 
     (pp' w (stream_bench d))
     (\i -> tell $ Counts (length i) (if i == "\n" then 1 else 0))

{-
benchS 2 7
-- Counts 16 2

benchS 2 2
-- Counts 16 3

benchS 15 10
-- Counts 414870 32767
-}


-- Building the tree first
mainD :: IO ()
mainD = getArgs >>= go
 where
 go [ds,ws] | [(d,"")] <- reads ds, 
              [(w,"")] <- reads ws = do
  print (d,w)
  let doc = doc_bench d
  putStrLn $ "Content " ++ show (doc_count doc) -- also actualizes the tree
  stat0 <- get_stat
  print (fmt_count doc w)
  stat1 <- get_stat
  putStrLn $ "Content " ++ show (doc_count doc) -- making sure tree is in memory
  print_stat stat1 stat0

 -- Print statistics. Must run the program with -T rts flag
 get_stat = performGC >> getGCStats
 print_stat s1 s0 = do
  -- putStrLn $ "before" ++ show s0
  -- putStrLn $ "after" ++ show s1
  putStrLn . (">>maxBytesUsed0 "++) . show $ maxBytesUsed s0
  putStrLn . (">>maxBytesUsed1 "++) . show $ maxBytesUsed s1
  putStrLn . (">>maxBytesUsed-currentUsed "++) . show $ 
    maxBytesUsed s1 - currentBytesUsed s1
  putStrLn . (">>Average-formatting "++) . show $
           ((fromIntegral (cumulativeBytesUsed s1 - cumulativeBytesUsed s0)) /
           (fromIntegral (numByteUsageSamples s1 - numByteUsageSamples s0)) -
    fromIntegral (currentBytesUsed s1))
  putStrLn . (">>peakMegabytesAllocated "++) . show $ peakMegabytesAllocated s1
  putStrLn . (">>numCGs "++) . show $ (numGcs s1 - numGcs s0)
  putStrLn . (">>mutatorCpuSeconds "++) . show $ 
        mutatorCpuSeconds s1 - mutatorCpuSeconds s0
  putStrLn . (">>gcCpuSeconds "++) . show $ 
        gcCpuSeconds s1 - gcCpuSeconds s0
  putStrLn . (">>cpuSeconds "++) . show $ 
        cpuSeconds s1 - cpuSeconds s0

-- Using the stream, without building the big tree
mainS :: IO ()
mainS = getArgs >>= go
 where
 go [ds,ws] | [(d,"")] <- reads ds, 
              [(w,"")] <- reads ws = do
  print (d,w)
  print (benchS d w)
  
-- Compile the code
-- ghc -O2 -rtsopts -main-is PPYield.mainD PPYield.hs
-- ghc -O2 -rtsopts -main-is PPYield.mainS PPYield.hs
-- To run this code
-- GHCRTS="-tstderr" ./PPYield 15 50
-- for w in 10 15 20 25 30 35 40 45 50 55; do for i in 1 2 3 4 5; do GHCRTS="-tstderr" ./PPYield 15 $w; done; echo "===="; done
-- for d in 10 11 12 13 14 15 16 17 18; do for i in 1 2 3 4 5; do GHCRTS="-tstderr" ./PPYield $d 50; done; echo "===="; done
