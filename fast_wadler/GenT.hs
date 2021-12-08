-- Simple implementation of generators
-- (essentially the implementation of CLU)
-- This implementation makes it clear how simple generators
-- can be implemented on a linear stack without copying

module GenT (GenT, yield, foldG, foldG_, mapG, runGenT, StateT) where

import Control.Monad.Reader
import Control.Monad.State

-- Very simple implemenation, which is patterened
-- after the implementation of iterators in CLU
-- We don't use the ContT monad. We only use the `mild'
-- effect, environment.

type GenT e m = ReaderT (e -> m ()) m

yield :: Monad m => e -> GenT e m ()
yield e = ask >>= \f -> lift $ f e

runGenT :: GenT e m () -> (e -> m ()) -> m ()
runGenT m f = runReaderT m f


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

-- Traverse the document tree and print out the node as we traversed them
-- Show in the XML-like way
traverse1 :: Doc -> IO ()
traverse1 d@(Text _) = putStrLn . show $ d
traverse1 d@Line     = putStrLn . show $ d
traverse1 (d1 :+: d2) = traverse1 d1 >> traverse1 d2
traverse1 (Group d)  = do
		       putStrLn "Begin Group"
		       traverse1 d
		       putStrLn "End Group"

tt1 = traverse1 doc1
{-
Begin Group
Text "A"
Line
Begin Group
Text "B"
Line
Text "C"
End Group
End Group
-}

-- Do the same with generators

-- We just globally replaced putStrLn with yield
-- (and changed the signature)

traverse2 :: Monad m => Doc -> GenT String m ()
traverse2 d@(Text _) = yield . show $ d
traverse2 d@Line     = yield . show $ d
traverse2 (d1 :+: d2) = traverse2 d1 >> traverse2 d2
traverse2 (Group d)  = do
		       yield "Begin Group"
		       traverse2 d
		       yield "End Group"

-- One should not be surprised in how we run this
-- (which instantiates yield as putStrLn)

tt2 = runGenT (traverse2 doc1) putStrLn
{-
Begin Group
Text "A"
Line
Begin Group
Text "B"
Line
Text "C"
End Group
End Group
-}

type Producer m e            = GenT e m ()
type Consumer m e            = e -> m ()
type Transducer m1 m2 e1 e2  = Producer m1 e1 -> Producer m2 e2

-- The analogue of map on generators (producers)
mapG :: Monad m => (e1 -> e2) -> Transducer (GenT e2 m) m e1 e2
mapG f gen = runGenT gen (yield . f)

-- A finite state Stream transducer: from the stream of e1
-- to the stream of e2, and the state s 

foldG :: Monad m =>
	 (s -> e -> m s) -> s -> GenT e (StateT s m) () -> m s
foldG f s0 gen = execStateT (runGenT gen consumer) s0
 where consumer x = get >>= (\s -> lift $ f s x) >>= put

-- A variant of foldG that returns no final state
foldG_ :: Monad m =>
	 (s -> e -> m s) -> s -> GenT e (StateT s m) () -> m ()
foldG_ f s0 gen = evalStateT (runGenT gen consumer) s0
 where consumer x = get >>= (\s -> lift $ f s x) >>= put


-- A small test of the online behavior
tf1 :: IO ()
tf1 = runGenT gen1 (\i -> putStrLn $ "Generated: " ++ show i)
 where gen = yield 1 >> yield 2
       gen1 = foldG f () gen
       f _ e = do
	       liftIO (putStrLn $ "f: " ++ show e)
	       yield e
	       return ()
{-
f: 1
Generated: 1
f: 2
Generated: 2
-}

-- A more interesting example, transforming the stream of strings
-- to the stream of running lengths of the strings

tranW :: MonadIO m => GenT String (StateT Int (GenT Int m)) () 
         -> GenT Int m () 
tranW = foldG_ f 0 
 where f acc str = do liftIO (putStr str >> putStr ": ")
		      let acc' = acc + length str
		      yield acc' >> return acc'

tW :: IO ()
tW = runGenT (tranW (traverse2 doc1)) print
{-
Begin Group: 11
Text "A": 19
Line: 23
Begin Group: 34
Text "B": 42
Line: 46
Text "C": 54
End Group: 63
End Group: 72
-}

