{-# LANGUAGE ScopedTypeVariables, Rank2Types, FlexibleInstances #-}

import System.Environment (getArgs)
import Data.List (intercalate, uncons)
import Data.Maybe (catMaybes)
import qualified Wadler as Wadler
import qualified Chitil as Chitil
import qualified Pombrio as Pombrio
import qualified Swierstra as Swierstra
import Generator

class GenericDoc d where
  nil :: d
  text :: String -> d
  line :: d
  (<>) :: d -> d -> d
  nest :: Int -> d -> d
  group :: d -> d
  (<|>) :: d -> d -> d
  flatten :: d -> d
  pretty :: Int -> d -> String

instance GenericDoc Wadler.DOC where
  nil = Wadler.nil
  text = Wadler.text
  line = Wadler.line
  (<>) = (Wadler.<>)
  nest = Wadler.nest
  group = Wadler.group
  (<|>) = (Wadler.:<|>)
  flatten = Wadler.flatten
  pretty = Wadler.pretty

instance GenericDoc Chitil.Doc where
  nil = Chitil.Doc id
  text = Chitil.text
  line = Chitil.line
  (<>) = (Chitil.<>)
  nest = Chitil.nestMargin
  group = Chitil.group
  (<|>) = undefined
  flatten = undefined
  pretty = Chitil.pretty

instance GenericDoc Pombrio.MDoc where
  nil = Pombrio.nil
  text = Pombrio.text
  line = Pombrio.line
  (<>) = (Pombrio.<>)
  nest = Pombrio.nest
  group = Pombrio.group
  (<|>) = (Pombrio.<|>)
  flatten = Pombrio.flatten
  pretty = Pombrio.pretty

instance GenericDoc Swierstra.Cont where
  nil = Swierstra.nil
  text = Swierstra.text
  line = Swierstra.line
  (<>) = (Swierstra.<>)
  nest = Swierstra.nest
  group = Swierstra.group
  (<|>) = undefined
  flatten = undefined
  pretty = Swierstra.pretty

data ShowDoc = ShowDoc String
unwrapShowDoc :: ShowDoc -> String
unwrapShowDoc (ShowDoc s) = s

-- For debug printing. Quadratic, but that's irrelevant.
instance GenericDoc ShowDoc where
  nil = ShowDoc "nil"
  text s = ShowDoc $ "\"" ++ s ++ "\""
  line = ShowDoc "NL"
  x <> y = ShowDoc $ "(" ++ unwrapShowDoc x ++ " <> " ++ unwrapShowDoc y ++ ")"
  nest i x = ShowDoc $ "(nest " ++ show i ++ " " ++ unwrapShowDoc x ++ ")"
  group x = ShowDoc $ "(group " ++ unwrapShowDoc x ++ ")"
  x <|> y = ShowDoc $ "(" ++ unwrapShowDoc x ++ " <|> " ++ unwrapShowDoc y ++ ")"
  flatten x = ShowDoc $ "(flatten " ++ unwrapShowDoc x ++ ")"
  pretty = undefined

showDoc :: DocMaker -> String
showDoc d = if length printed > 200
            then "[large doc omitted]"
            else printed
  where
    printed = unwrapShowDoc (makeDoc d :: ShowDoc)

-- Generate all docs of the given size
gDoc :: Gen DocMaker
gDoc = gDelay $ gNil <||> gText <||> gLine <||> gConcat <||> gNest <||> gChoice <||> gFlatten
  where
    gNil = gVal (DocMaker nil)
    gText = gVal (DocMaker (text "a")) <||>
            gSize 1 (gVal (DocMaker (text "bb"))) <||>
            gSize 2 (gVal (DocMaker (text "ccc")))
    gLine = gVal (DocMaker line)
    gConcat = gSize 1 $ gMap (\(x, y) -> DocMaker (makeDoc x <> makeDoc y))
                             (gPair gDoc gDoc)
    gNest = gSize 1 $ gMap (\(i, x) -> DocMaker (nest i (makeDoc x)))
                           (gPair (gMap (\n -> n + 1) gNat) gDoc)
    -- gGroup = gSize 1 $ gMap (\x -> DocMaker (group (makeDoc x))) gDoc
    gChoice = gSize 1 $ gMap (\(x, y) -> DocMaker (makeDoc x <|> makeDoc y))
                             (gPair gDoc gDoc)
    gFlatten = gSize 1 $ gMap (\x -> DocMaker (flatten (makeDoc x))) gDoc

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

-- XML Notation, from Wadler's paper

data XML = Elt String [Att] [XML]
         | Txt String
data Att = Att String String

folddoc f [] = nil
folddoc f [x] = x
folddoc f (x:xs) = f x (folddoc f xs)

showXML :: GenericDoc d => XML -> d
showXML x = folddoc (<>) (showXMLs x)

showXMLs :: GenericDoc d => XML -> [d]
showXMLs (Elt n a []) = [text "<" <> showTag n a <> text "/>"]
showXMLs (Elt n a c)  = [text "<" <> showTag n a <> text ">" <>
                         showFill showXMLs c <>
                         text "</" <> text n <> text ">"]
showXMLs (Txt s) = map text (words s)

showAtts :: GenericDoc d => Att -> [d]
showAtts (Att n v) = [text n <> text "=" <> text (quoted v)]

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

showTag :: GenericDoc d => String -> [Att] -> d
showTag n a = text n <> showFill showAtts a

fill :: GenericDoc d => [d] -> d
fill [] = nil
fill [x] = x
fill (x:y:zs) = (flatten x <+> fill (flatten y : zs)) <|> (x </> fill (y : zs))

x <+> y = x <> text " " <> y
x </> y = x <> line <> y

showFill f [] = nil
showFill f xs = bracket "" (fill (concat (map f xs))) ""

bracket :: GenericDoc d => String -> d -> String -> d
bracket l x r = group (text l <> nest 2 (line <> x) <> line <> text r)

(<+/>) :: GenericDoc d => d -> d -> d
x <+/> y = x <> (text " " <|> line) <> y

-- Various DOCs that print slowly, some more devious than others.

data DocMaker = DocMaker (forall d. GenericDoc d => d)

makeDoc :: GenericDoc d => DocMaker -> d
makeDoc (DocMaker d) = d

type Width = Int
data Example = Example DocMaker Width

huge :: GenericDoc d => Int -> d
huge 0 = nil
huge n = let d = huge (n - 1) in d <> d

incremental :: GenericDoc d => Int -> d
incremental 0 = text "leaf(size = 0)"
incremental n = group (text ("branch(size = " ++ show (2 ^ n) ++ "):")
                       <> nest 4 (line <> subdoc <> line <> subdoc)
                       <> line <> text "end")
  where subdoc = incremental (n - 1)

antagonistic :: GenericDoc d => Int -> d
antagonistic 0 = text "line"
antagonistic n = group (antagonistic (n - 1) <> line <> text "line")

tnestedLists :: GenericDoc d => Int -> d
tnestedLists 0 = text "[]"
tnestedLists n = group (text "[" <> (nestedLists (n - 1)) <> text "]")

-- TODO: temporary
nestedLists :: GenericDoc d => Int -> d
nestedLists _ = text "a" <> text "a" <> flatten (text "a" <|> nil)

chitilDoc :: GenericDoc d => Int -> d
chitilDoc 0 = text ""
chitilDoc n = group (text "*" <> line <> chitilDoc (n - 1))

chitil :: GenericDoc d => d
chitil = foldr1 (\x y -> x <> line <> y) $ take 500 $ repeat $ chitilDoc 200

wadlerXml :: GenericDoc d => d
wadlerXml = showXML $
  Elt "p" [Att "color" "red", Att "font" "Times", Att "size" "10"] [
    Txt "Here is some",
    Elt "em" [] [Txt "emphasized"],
    Txt "text.",
    Txt "Here is a",
    Elt "a" [Att "href" "http://www.egg.com/"] [Txt "link"],
    Txt "elsewhere."
  ]

xml :: forall d. GenericDoc d => Int -> d
xml n = showXML $ makeXML n
  where
    makeXML :: Int -> XML
    makeXML 0 = Elt "leaf" [] []
    makeXML n = Elt "branch" [Att "size" (if n > 3 then "big" else "small"),
                              Att "numChildren" "2"]
                  (map makeXML (init [0..n]))

-- Testing functions

data Type a = Type

runPretty :: forall d. GenericDoc d => Type d -> String -> Int -> IO ()
runPretty _ which size = case which of
  "huge"         -> putStrLn $ pretty 10 (huge size :: d)
  "exponential"  -> putStrLn $ pretty 80 (incremental size :: d)
  "incremental"  -> putStrLn $ intercalate "\n" $ take 10 $ split '\n'
                    $ pretty 80 (incremental size :: d)
  -- TODO: fix width to (2 * size)
  "nestedLists"  -> putStrLn $ pretty 2 (nestedLists size :: d)
  "antagonistic" -> putStrLn $ pretty 10 (antagonistic size :: d)
  "chitil"       -> putStrLn $ show $ length $ pretty size (chitil :: d)
  "wadlerXml"    -> putStrLn $ pretty size (wadlerXml :: d)
  "xml"          -> putStrLn $ pretty 80 (xml size :: d)
  "all"          -> do
    putStrLn $ "Running on all " ++ show (qCount gDoc size) ++
               " docs of size " ++ show size
    mapM_ (\d -> putStrLn $ pretty 2 $ (makeDoc d :: d)) (qList gDoc size)
  "random"       -> do
    putStrLn $ "Running on 10000 random docs of size " ++ show size
    docs <- sequence $ take 10000 $ repeat $ qRandom gDoc size
    mapM_ (\d -> putStrLn $ pretty 5 $ (makeDoc d :: d)) docs
  _ -> error "DOC not recognized"
  where
    split c [] = [[]]
    split c (x:xs) | c == x = [] : split c xs
    split c (x:xs) = let (w:ws) = split c xs in ((x:w):ws)

compareToWadler :: forall d. GenericDoc d => Type d -> String -> Int -> IO ()
compareToWadler t which size = case which of
  "huge"         -> display $ matchesWadler t 10 (DocMaker (huge size))
  "nestedLists"  -> display $ matchesWadler t (2 * size) (DocMaker (nestedLists size))
  "exponential"  -> display $ matchesWadler t 80 (DocMaker (incremental size))
  "incremental"  -> display $ matchesWadler t 80 (DocMaker (incremental size))
  "antagonistic" -> display $ matchesWadler t 10 (DocMaker (antagonistic size))
  "chitil"       -> display $ matchesWadler t size (DocMaker chitil)
  "wadlerXml"    -> display $ matchesWadler t size (DocMaker wadlerXml)
  "xml"          -> display $ matchesWadler t 80 (DocMaker (xml size))
  "all"          -> display $ first $ catMaybes $
                      map (\d -> matchesWadler t 2 d) (qList gDoc size)
  "random"       -> do
    docs <- sequence $ take 10000 $ repeat $ qRandom gDoc size
    display $ first $ catMaybes $ map (\d -> matchesWadler t 5 d) docs
  _              -> error "DOC not recognized"

  where
    display Nothing = putStrLn "pass"
    display (Just (d, expected, actual)) = do
      putStrLn $ "FAILED on doc " ++ showDoc d
      putStrLn $ "EXPECTED:\n" ++ expected
      putStrLn $ "ACTUAL:\n" ++ actual
      putStrLn "END"
      putStrLn ""
  
    first [] = Nothing
    first (x : xs) = Just x

matchesWadler :: forall d. GenericDoc d => Type d -> Int -> DocMaker -> Maybe (DocMaker, String, String)
matchesWadler _ w d =
  if pretty w dWadler == pretty w dOther
  then Nothing
  else Just (d, pretty w dWadler, pretty w dOther)
  where
    dWadler :: Wadler.DOC = makeDoc d
    dOther :: d = makeDoc d

main = do
  args <- getArgs
  if length args /= 4
  then do
    putStrLn "Usage: ./benchmark [CMD] [IMPL] [DOC] size"
    putStrLn "where CMD is 'run' or 'test'"
    putStrLn "and IMPL is 'Wadler', 'Chitil', 'Swierstra', or 'Pombrio'"
    putStrLn "and DOC is 'all', 'random', 'huge', 'antagonistic', 'nestedLists', 'incremental', 'exponential', 'wadlerXml', 'xml', or 'chitil'"
  else
    let cmd = args !! 0
        impl = args !! 1
        which = args !! 2
        size = (read $ args !! 3) :: Int
    in case cmd of
      "run" -> case impl of
        "Wadler"    -> runPretty (Type :: Type Wadler.DOC) which size
        "Chitil"    -> runPretty (Type :: Type Chitil.Doc) which size
        "Swierstra" -> runPretty (Type :: Type Swierstra.Cont) which size
        "Pombrio"   -> runPretty (Type :: Type Pombrio.MDoc) which size
        _           -> error "IMPL not recognized"
      "test" -> case impl of
        "Wadler"    -> compareToWadler (Type :: Type Wadler.DOC) which size
        "Chitil"    -> compareToWadler (Type :: Type Chitil.Doc) which size
        "Swierstra" -> compareToWadler (Type :: Type Swierstra.Cont) which size
        "Pombrio"   -> compareToWadler (Type :: Type Pombrio.MDoc) which size
        _           -> error "IMPL not recognized"
      _ -> error "CMD not recognized"
