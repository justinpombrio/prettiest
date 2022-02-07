{-# LANGUAGE ScopedTypeVariables, Rank2Types, FlexibleInstances #-}

import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Wadler as Wadler
import qualified Chitil as Chitil
import qualified Pombrio as Pombrio
import qualified Swierstra as Swierstra
import qualified Kiselyov as Kiselyov
import Generator

class GenericDoc d where
  nil :: d
  text :: String -> d
  line :: d
  (<>) :: d -> d -> d
  nest :: Int -> d -> d
  group :: d -> d
  pretty :: Int -> d -> String

  (<|>) :: d -> d -> d
  flatten :: d -> d

instance GenericDoc Wadler.DOC where
  nil = Wadler.nil
  text = Wadler.text
  line = Wadler.line
  (<>) = (Wadler.<>)
  nest = Wadler.nest
  group = Wadler.group
  pretty = Wadler.pretty

  (<|>) = (Wadler.:<|>)
  flatten = Wadler.flatten

instance GenericDoc Chitil.Doc where
  nil = Chitil.Doc id
  text = Chitil.text
  line = Chitil.line
  (<>) = (Chitil.<>)
  nest = Chitil.nestMargin
  group = Chitil.group
  pretty = Chitil.pretty

  (<|>) = undefined
  flatten = undefined

instance GenericDoc Pombrio.MDoc where
  nil = Pombrio.nil
  text = Pombrio.text
  line = Pombrio.line
  (<>) = (Pombrio.<>)
  nest = Pombrio.nest
  group = Pombrio.group
  pretty = Pombrio.pretty

  (<|>) = (Pombrio.<|>)
  flatten = Pombrio.flatten

instance GenericDoc Swierstra.Cont where
  nil = Swierstra.nil
  text = Swierstra.text
  line = Swierstra.line
  (<>) = (Swierstra.<>)
  nest = Swierstra.nest
  group = Swierstra.group
  pretty = Swierstra.pretty

  (<|>) = undefined
  flatten = undefined

instance GenericDoc Kiselyov.Doc where
  nil = Kiselyov.Text ""
  text = Kiselyov.Text
  line = Kiselyov.Line
  (<>) = (Kiselyov.:+:)
  nest = undefined
  group = Kiselyov.Group
  pretty = undefined -- TODO

  (<|>) = undefined
  flatten = undefined

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

showOutput :: String -> String
showOutput o = if length o > 1000 then "[large output omitted]" else o

showDoc :: DocMaker -> String
showDoc d = showOutput $ unwrapShowDoc $ (makeDoc d :: ShowDoc)

-- Generate all docs of the given size
gDoc :: Gen DocMaker
gDoc = gDelay $ gNil <||> gText <||> gLine <||> gConcat <||> gNest <||> gGroup <||> gFill
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

    gGroup = gSize 1 $ gMap (\x -> DocMaker (group (makeDoc x))) gDoc
    gFill = gSize 4 $ gMap (\(x, (y, z)) -> DocMaker (fill [makeDoc x, makeDoc y, makeDoc z]))
                           (gPair gDoc (gPair gDoc gDoc))

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
fill (x:y:zs) = (flatten x <++> fill (flatten y : zs)) <|> (x </> fill (y : zs))

fillwords :: GenericDoc d => String -> d
fillwords = folddoc (<+/>) . map text . words

x <++> y = x <> text " " <> y
x </> y = x <> line <> y

showFill f [] = nil
showFill f xs = bracket "" (fill (concat (map f xs))) ""

bracket :: GenericDoc d => String -> d -> String -> d
bracket l x r = group (text l <> nest 2 (line <> x) <> line <> text r)

(<+/>) :: GenericDoc d => d -> d -> d
-- The paper said this, but some printers only support `group` and not `<|>`
-- x <+/> y = x <> (text " " <|> line) <> y
x <+/> y = x <> group line <> y

-- Various DOCs that print slowly, some more devious than others.

data DocMaker = DocMaker (forall d. GenericDoc d => d)

makeDoc :: GenericDoc d => DocMaker -> d
makeDoc (DocMaker d) = d

type Width = Int
type Size = Int
type NumLines = Maybe Int

data Example = Example (forall d. GenericDoc d => d) Width NumLines
data Examples = Examples [Example] String

huge :: Size -> Examples
huge n = Examples [Example (hugeDoc n) 10 Nothing]
         "huge         -- A doc of size 2^n that is entirely empty"
  where
    hugeDoc 0 = nil
    hugeDoc n = let d = hugeDoc (n - 1) in d <> d

incremental :: Size -> Examples
incremental n = Examples [Example (expDoc n) 80 (Just 10)]
                "incremental  -- Just the first 10 lines of a doc of size ~2^n"

exponential :: Size -> Examples
exponential n = Examples [Example (expDoc n) 80 Nothing]
                "exponential  -- A tree-shaped doc of size ~2^n"

expDoc 0 = text "leaf(size = 0)"
expDoc n = group (text ("branch(size = " ++ show (2 ^ n) ++ "):")
                <> nest 4 (line <> subdoc1 <> line <> subdoc2)
                <> line <> text "end")
  where subdoc1 = expDoc (n - 1)
        subdoc2 = expDoc (n - 1)

antagonistic :: Size -> Examples
antagonistic n = Examples [Example (ant n) 10 Nothing]
                 "antagonistic -- A doc designed to foil Wadler's printer by taking time O(n^2)"
  where
    ant 0 = text "line"
    ant n = group (ant (n - 1) <> line <> text "line")

-- TODO: put some optional newlines in here?
nestedLists :: Size -> Examples
nestedLists n = Examples [Example (nested n) (2 * n) Nothing]
                "nestedLists  -- Deeply nested lists: nested depth is n, printing width is 2*n"
  where
    nested 0 = text "[]"
    nested n = group (text "[" <> (nested (n - 1)) <> text "]")

chitil :: Size -> Examples
chitil n = Examples [Example doc n Nothing]
           "chitil       -- A doc from Chitil, designed to foil Wadler's printer by taking time O(nw)"
  where
    doc :: GenericDoc d => d
    doc = foldr1 (\x y -> x <> line <> y) $ take 500 $ repeat $ chunk 200

    chunk :: GenericDoc d => Int -> d
    chunk 0 = text ""
    chunk n = group (text "*" <> line <> chunk (n - 1))

wadlerXml :: Size -> Examples
wadlerXml n = Examples [Example doc n Nothing]
              "wadlerXml    -- A fixed-size example from Wadler's paper. n is the printing width"
  where
    doc :: GenericDoc d => d
    doc = showXML $
      Elt "p" [Att "color" "red", Att "font" "Times", Att "size" "10"] [
        Txt "Here is some",
        Elt "em" [] [Txt "emphasized"],
        Txt "text.",
        Txt "Here is a",
        Elt "a" [Att "href" "http://www.egg.com/"] [Txt "link"],
        Txt "elsewhere."
      ]

xml :: Size -> Examples
xml n = Examples [Example (showXML $ makeXML n) 80 Nothing]
        "xml          -- An xml doc of size ~2^n, printed at width 80"
  where
    makeXML :: Int -> XML
    makeXML 0 = Elt "leaf" [] []
    makeXML n = Elt "branch" [Att "size" (if n > 3 then "big" else "small"),
                              Att "numChildren" "2"]
                  (map makeXML (init [0..n]))

wordflow :: Size -> Examples
wordflow n = Examples [Example (doc n) 80 Nothing]
       "flow          -- Word wrap n words of Lorem Ipsum, printed at width 80"
  where
    doc :: GenericDoc d => Int -> d
    doc n = fillwords $ intercalate " " $ take n $ cycle ipsum
    ipsum = words "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

nonOptimal :: Size -> Examples
nonOptimal w = Examples [Example nopt w Nothing]
             "nonOptimal   -- Demonstrate that Wadler does not minimize height"
  where
    nopt :: GenericDoc d => d
    nopt = group (text "AA" <> group line <> text "A") <>
           nest 4 (group (text "B" <> line <> text "B" <> line <> text "B"))

allExample :: Size -> Examples
allExample size = Examples examples
                  "all          -- All docs of size n (gets very large after n>5)"
  where
    examples = [Example d (goodWidth size) Nothing | DocMaker d <- qList gDoc size]

random :: Size -> IO Examples
random size = do
  docs <- sequence $ take 10000 $ repeat $ qRandom gDoc size
  let examples = [Example d (goodWidth size) Nothing | DocMaker d <- docs]
  return $ Examples examples
           "random       -- 10,000 docs of size n chosen uniformly at random"

goodWidth :: Size -> Width
goodWidth n | n < 3  = 1
            | n < 6  = 2
            | n < 10 = 3
            | True   = 5

lookupExamples :: String -> Int -> IO Examples
lookupExamples "huge"         size = return $ huge         size
lookupExamples "exponential"  size = return $ exponential  size
lookupExamples "incremental"  size = return $ incremental  size
lookupExamples "nestedLists"  size = return $ nestedLists  size
lookupExamples "antagonistic" size = return $ antagonistic size
lookupExamples "chitil"       size = return $ chitil       size
lookupExamples "wadlerXml"    size = return $ wadlerXml    size
lookupExamples "xml"          size = return $ xml          size
lookupExamples "wordflow"     size = return $ wordflow     size
lookupExamples "all"          size = return $ allExample   size
lookupExamples "nonOptimal"   size = return $ nonOptimal   size
lookupExamples "random"       size = random size
lookupExamples which          _    = error ("DOC " ++ which ++ " not recognized")

allExamples = ["all", "random", "huge", "exponential", "incremental", "nestedLists",
               "antagonistic", "chitil", "wadlerXml", "xml", "nonOptimal"]

-- Testing functions

data Type a = Type

runExample :: forall d. GenericDoc d => Type d -> Example -> String
runExample _ (Example d w Nothing) = pretty w (d :: d)
runExample _ (Example d w (Just l)) =
  intercalate "\n" $ take l $ split '\n' $ pretty w (d :: d)
  where
    split c [] = [[]]
    split c (x:xs) | c == x = [] : split c xs
    split c (x:xs) = let (w:ws) = split c xs in ((x:w):ws)

runPretty :: forall d. GenericDoc d => Type d -> String -> Int -> IO ()
runPretty t which size = do
  (Examples examples _) <- lookupExamples which size
  sequence_ (map (putStrLn . runExample t) examples)

compareToWadler :: forall d. GenericDoc d => Type d -> String -> Int -> IO ()
compareToWadler t which size = do
  Examples examples _ <- lookupExamples which size
  putStrLn $ display $ first $ catMaybes $ map (matchesWadler t) examples
  where
    display Nothing = "pass"
    display (Just msg) = msg
  
    first [] = Nothing
    first (x : xs) = Just x

matchesWadler :: forall d. GenericDoc d => Type d -> Example -> Maybe String
matchesWadler t ex@(Example d w _) =
  let wadlerOut = runExample (Type :: Type Wadler.DOC) ex
      otherOut  = runExample t ex in
  if wadlerOut == otherOut
  then Nothing
  else Just ("FAILED at width " ++ show w ++ " on doc " ++ showDoc (DocMaker d) ++ "\n" ++
             "EXPECTED:\n" ++ showOutput wadlerOut ++ "\n" ++
             "ACTUAL:\n" ++ showOutput otherOut ++ "\n" ++
             "END\n\n")

main = do
  args <- getArgs
  if length args /= 4
  then do
    putStrLn "Run a variety of pretty printers on a variety of example docs"
    putStrLn "Compilation: ghc --make RunPretty"
    putStrLn ""
    putStrLn "Usage: ./RunPretty [CMD] [IMPL] [DOC] size"
    putStrLn ""
    putStrLn "CMD can be:"
    putStrLn "  run  -- pretty print the DOC to stdout"
    putStrLn "  test -- compare the IMPL's output on DOC to that of Wadler's. Fail if it disagrees with Wadler"
    putStrLn ""
    putStrLn "IMPL can be:"
    putStrLn "  Wadler    -- A Prettier Printer, Philip Wadler, JFP, 1998"
    putStrLn "  Chitil    -- Pretty Printing with Lazy Dequeues, Olaf Chitil, 2005"
    putStrLn "  Swierstra -- Linear, Bounded, Functional Pretty Printing, Swierstra and Chitil, 2009"
    putStrLn "  Pombrio   -- This work"
    putStrLn ""
    putStrLn "DOC can be:"
    flip mapM_ allExamples $ \which -> do
      Examples _ desc <- lookupExamples which 1
      putStrLn $ "  " ++ desc
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
