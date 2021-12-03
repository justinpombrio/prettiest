{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

import System.Environment (getArgs)
import qualified Wadler as Wadler
import qualified Chitil as Chitil
import qualified Pombrio as Pombrio
import Generator

class GenericDoc d where
  nil :: d
  text :: String -> d
  line :: d
  (<>) :: d -> d -> d
  nest :: Int -> d -> d
  group :: d -> d
  pretty :: Int -> d -> String

instance GenericDoc Wadler.DOC where
  nil = Wadler.nil
  text = Wadler.text
  line = Wadler.line
  (<>) = (Wadler.<>)
  nest = Wadler.nest
  group = Wadler.group
  pretty = Wadler.pretty

instance GenericDoc Chitil.Doc where
  nil = Chitil.Doc id
  text = Chitil.text
  line = Chitil.line
  (<>) = (Chitil.<>)
  nest = Chitil.nestMargin
  group = Chitil.group
  pretty = Chitil.pretty

instance GenericDoc Pombrio.Doc where
  nil = Pombrio.nil
  text = Pombrio.text
  line = Pombrio.line
  (<>) = (Pombrio.<>)
  nest = Pombrio.nest
  group = Pombrio.group
  pretty = Pombrio.pretty

data ShowDoc = ShowDoc String
showDoc :: ShowDoc -> String
showDoc (ShowDoc s) = s

-- for printing
instance GenericDoc ShowDoc where
  nil = ShowDoc "nil"
  text s = ShowDoc $ "\"" ++ s ++ "\""
  line = ShowDoc "NL"
  x <> y = ShowDoc $ "(" ++ showDoc x ++ " <> " ++ showDoc y ++ ")"
  nest i x = ShowDoc $ "(nest " ++ show i ++ " " ++ showDoc x ++ ")"
  group x = ShowDoc $ "(group " ++ showDoc x ++ ")"
  pretty = undefined

-- Generate all docs of the given size
gDoc :: Gen DocMaker
gDoc = gDelay $ gNil <||> gText <||> gLine <||> gConcat <||> gNest <||> gGroup
  where
    gNil = gVal (DocMaker nil)
    gText = gVal (DocMaker (text "a")) <||>
            gSize 1 (gVal (DocMaker (text "bb"))) <||>
            gSize 2 (gVal (DocMaker (text "ccc")))
    gLine = gVal (DocMaker line)
    gConcat = gMap (\(x, y) -> DocMaker (makeDoc x <> makeDoc y))
                   (gPair gDoc gDoc)
    gNest = gMap (\(i, x) -> DocMaker (nest i (makeDoc x)))
                 (gPair gNat gDoc)
    gGroup = gSize 1 $ gMap (\x -> DocMaker (group (makeDoc x))) gDoc
-- gDoc :: forall d. GenericDoc d => Gen d
-- gDoc = gDelay $ gNil <||> gText <||> gLine <||> gConcat <||> gNest <||> gGroup
--   where
--     gNil = gVal nil
--     gText = gVal (text "a") <||>
--             gSize 1 (gVal (text "bb")) <||>
--             gSize 2 (gVal (text "ccc"))
--     gLine = gVal line
--     gConcat = gMap (uncurry (<>)) (gPair gDoc gDoc)
--     gNest = gMap (uncurry nest) (gPair gNat gDoc)
--     gGroup = gSize 1 $ gMap group gDoc

data DocMaker = DocMaker (forall d. GenericDoc d => d)

makeDoc :: GenericDoc d => DocMaker -> d
makeDoc (DocMaker d) = d

-- A counter example that proves that the condition required for using :<|> given in Wadler's paper is
-- incorrect. Though the correct condition is _also_ obeyed by the constructs in the paper that use
-- :<|> (such as `group`), so the proofs in the paper stand for the DOC constructors given in the
-- paper. You just need to be careful if you extend the printer by adding a new constructor that
-- uses :<|>.

counterEx :: Wadler.DOC
counterEx = line <> group ab
  where ab = text "a" <> line <> text "b"

counterEx' :: Wadler.DOC
counterEx' = (line <> Wadler.flatten ab) Wadler.:<|> (line <> ab)
  where ab = text "a" <> line <> text "b"

runCounterExamples :: IO ()
runCounterExamples = do
  putStrLn "------counterEx"
  putStrLn $ pretty 2 counterEx
  putStrLn "------counterEx'"
  putStrLn $ pretty 2 counterEx'
  putStrLn "------"

-- Various DOCs that print slowly, some more devious than others.

huge :: GenericDoc d => Int -> d
huge 0 = nil
huge n = let d = huge (n - 1) in d <> d

antagonistic :: GenericDoc d => Int -> d
antagonistic 0 = text "line"
antagonistic n = group (antagonistic (n - 1) <> line <> text "line")

nestedLists :: GenericDoc d => Int -> d
nestedLists 0 = text "[]"
nestedLists n = group (text "[" <> (nestedLists (n - 1)) <> text "]")

chitilDoc :: GenericDoc d => Int -> d
chitilDoc 0 = text ""
chitilDoc n = group (text "*" <> line <> chitilDoc (n - 1))

chitil :: GenericDoc d => d
chitil = foldr1 (\x y -> x <> line <> y) $ take 500 $ repeat $ chitilDoc 200

data Type a = Type

runPretty :: forall d. GenericDoc d => Type d -> String -> Int -> IO ()
runPretty _ which size = case which of
  "huge"         -> putStrLn $ pretty 10 (huge size :: d)
  "nestedLists"  -> putStrLn $ pretty (2 * size) (nestedLists size :: d)
  "antagonistic" -> putStrLn $ pretty 10 (antagonistic size :: d)
  "chitil"       -> putStrLn $ show $ length $ pretty size (chitil :: d)
  "all"          -> do
    putStrLn $ "Running on all " ++ show (qCount size gDoc) ++
               " docs of size " ++ show size
    mapM_ (\d -> putStrLn $ pretty 20 $ (makeDoc d :: d)) (qList size gDoc)
  _ -> error "DOC not recognized"

compareToWadler :: forall d. GenericDoc d => Type d -> String -> Int -> IO ()
compareToWadler t which size =
  case result of
    True  -> putStrLn "pass"
    False -> putStrLn "fail"
  where
    result = case which of
      "huge"         -> matchesWadler t 10 (DocMaker (huge size))
      "nestedLists"  -> matchesWadler t (2 * size) (DocMaker (nestedLists size))
      "antagonistic" -> matchesWadler t 10 (DocMaker (antagonistic size))
      "chitil"       -> matchesWadler t size (DocMaker chitil)
      "all"          -> all (\d -> matchesWadler t 20 d) (qList size gDoc)
      _              -> error "DOC not recognized"

matchesWadler :: forall d. GenericDoc d => Type d -> Int -> DocMaker -> Bool
matchesWadler _ w d = pretty w dWadler == pretty w dOther
  where
    dWadler :: Wadler.DOC = makeDoc d
    dOther :: d = makeDoc d

main = do
  args <- getArgs
  if length args /= 4
  then do
    putStrLn "Usage: ./benchmark [CMD] [IMPL] [DOC] size"
    putStrLn "where CMD is 'run' or 'test'"
    putStrLn "and IMPL is 'Wadler', 'Chitil', or 'Pombrio'"
    putStrLn "and DOC is 'all', 'huge', 'antagonistic', 'nestedLists', or 'chitil'"
  else
    let cmd = args !! 0
        impl = args !! 1
        which = args !! 2
        size = (read $ args !! 3) :: Int
    in case cmd of
      "run" -> case impl of
        "Wadler"  -> runPretty (Type :: Type Wadler.DOC) which size
        "Chitil"  -> runPretty (Type :: Type Chitil.Doc) which size
        "Pombrio" -> runPretty (Type :: Type Pombrio.Doc) which size
        _         -> error "IMPL not recognized"
      "test" -> case impl of
        "Wadler"  -> compareToWadler (Type :: Type Wadler.DOC) which size
        "Chitil"  -> compareToWadler (Type :: Type Chitil.Doc) which size
        "Pombrio" -> compareToWadler (Type :: Type Pombrio.Doc) which size
        _         -> error "IMPL not recognized"
      _ -> error "CMD not recognized"
