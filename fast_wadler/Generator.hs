module Generator where

import System.Random (randomRIO)

-- |A generator of values of type a. Each value has a _size_, and you can ask the generator for all
-- |values of a particular size, or for a uniformly random value of a particular size, or related
-- |questions.

-- In `Gen g cs`:
-- * `g` is a function from size n to index i to the i'th value of size n,
-- * and `cs` is a list, whose n'th element is the number of values of size n.
data Gen a = Gen (Int -> Integer -> a) [Integer]

{----------------}
{- Constructors -}
{----------------}

-- |A generator that makes values from the set; the values will all be considered to have size 0.
-- |The set must be finite.
gSet :: [a] -> Gen a
gSet vals =
  Gen (\n i -> if n == 0 then vals !! (fromInteger i) else error "bug in gen library")
      (toInteger (length vals) : repeat 0)

-- |A generator that makes only a single value, considered to be size 0.
gVal :: a -> Gen a
gVal v =
  Gen (\n i -> if n == 0 && i == 0 then v else error "bug in gen library")
      (toInteger 1 : repeat 0)

-- |Take two generators, and produce a generator over all possible pairs of their values.
gPair :: Gen a -> Gen b -> Gen (a, b)
gPair (Gen g1 c1) (Gen g2 c2) =
  Gen (\n i -> index i [(i, n - i) | i <- [0..n]])
      (map (\n -> if n < 0
                  then 0
                  else sum [c1 !! i * c2 !! (n - i) | i <- [0 .. n]])
           [0..])
  where
    index i [] = error "bug in gen library"
    index i ((n, m) : sizes) =
      let size = (c1 !! n) * (c2 !! m)
      in if i < size
         then (g1 n (i `div` (c2 !! m)), g2 m (i `mod` (c2 !! m)))
         else index (i - size) sizes

-- |Take two generators, and produce a generator over the union of their values.
gOr :: Gen a -> Gen a -> Gen a
gOr (Gen g1 cs1) (Gen g2 cs2) =
  Gen (\n -> let size = cs1 !! n
             in \i -> if i < size
                      then g1 n i
                      else g2 n (i - size))
      (zipWith (+) cs1 cs2)

infixl 2 <||>
(<||>) = gOr

-- |Increase the size of the values of a generator.
gSize :: Int -> Gen a -> Gen a
gSize m (Gen g cs) | m >= 0 =
  Gen (\n -> g (n - m))
      (replicate m 0 ++ cs)

-- |Modify the values produces by a generator.
gMap :: (a -> b) -> Gen a -> Gen b
gMap f (Gen g cs) = Gen (\n i -> f (g n i)) cs

-- |Wrap this around recursive generators, so that they terminate.
gDelay :: Gen a -> Gen a
gDelay gen = Gen (\n -> let Gen g _ = gen in g n) (let Gen _ c = gen in c)

-- |A generator over natural numbers, where number n has size n.
gNat :: Gen Int
gNat = Gen (\n i -> if i == 0 then n else error "bug in gen library") (repeat 1)

{-----------}
{- Queries -}
{-----------}

-- A (possibly infinite) list of all generated values, in order of size.
qAll :: Gen a -> [a]
qAll g = concatMap (qList g) [0..]

-- List all values of the given size
qList :: Gen a -> Int -> [a]
qList (Gen g cs) n = map (g n) (init [0 .. cs!!n])

-- Compute the number of values of the given size
qCount :: Gen a -> Int -> Integer
qCount (Gen _ cs) n = cs !! n

-- Generate a value of the given size uniformly at random
qRandom :: Gen a -> Int -> IO a
qRandom (Gen g cs) n =
  let size = cs !! n - 1
  in if size == 0
     then error ("there are no values of size " ++ show size)
     else do
       i <- randomRIO (0, size - 1)
       return (g n i)

-- Generate values of the given size uniformly at random
qRandoms :: Gen a -> Int -> IO [a]
qRandoms g n = sequence (repeat (qRandom g n))
