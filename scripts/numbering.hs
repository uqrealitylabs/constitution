import Data.Char qualified as C
import Data.List qualified as L
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

trim :: String -> String
trim = f . f where f = reverse . dropWhile C.isSpace

startsWith :: String -> String -> Bool
startsWith p s = p `L.isPrefixOf` s

lower :: String -> String
lower = map C.toLower

romanToInt :: String -> Int
romanToInt s = go 0 0 (reverse $ lower s)
  where
    v 'i' = 1
    v 'v' = 5
    v 'x' = 10
    v 'l' = 50
    v 'c' = 100
    v 'd' = 500
    v 'm' = 1000
    v _ = 0
    go _ acc [] = acc
    go prev acc (c : cs) =
      let vi = v c in go (max prev vi) (acc + if vi < prev then -vi else vi) cs

intToRoman :: Int -> String
intToRoman n = build n table
  where
    table =
      [ (1000, "m"),
        (900, "cm"),
        (500, "d"),
        (400, "cd"),
        (100, "c"),
        (90, "xc"),
        (50, "l"),
        (40, "xl"),
        (10, "x"),
        (9, "ix"),
        (5, "v"),
        (4, "iv"),
        (1, "i")
      ]
    build 0 _ = ""
    build k ((v, s) : ts)
      | k >= v = s ++ build (k - v) ((v, s) : ts)
      | otherwise = build k ts
    build _ [] = ""

data LineTag
  = PartHdr
  | Top Int Char
  | Sub Int Char String
  | SubSub Int Char String Int
  | Other

digits :: String -> (String, String)
digits = span C.isDigit

parseLine :: String -> LineTag
parseLine raw =
  let s = trim raw
   in if startsWith "## Part" s
        then PartHdr
        else case dropWhile C.isSpace s of
          ('*' : '*' : rest) ->
            case digits rest of
              (d, l : '.' : '*' : '*' : _) | not (null d) && C.isLower l -> Top (read d) l
              _ -> Other
          ('-' : ' ' : '*' : '*' : rest) -> parseBullet rest
          ('*' : ' ' : '*' : '*' : rest) -> parseBullet rest
          _ -> Other
  where
    parseBullet rest =
      case digits rest of
        (d, l : '.' : after)
          | not (null d) && C.isLower l ->
              let body = takeWhile (/= '*') after
               in if null body || last body /= '.'
                    then Other
                    else
                      let body' = init body
                       in case break (== '.') body' of
                            (r, "") -> Sub (read d) l r
                            (r, '.' : n) | all C.isDigit n -> SubSub (read d) l r (read n)
                            _ -> Other
        _ -> Other

data State = S
  { curPart :: Maybe Int,
    expLetter :: Maybe Char,
    expRoman :: Maybe String,
    expNum :: Maybe Int,
    parent :: Maybe (Int, Char),
    errs :: [String]
  }

emptyS :: State
emptyS = S Nothing Nothing Nothing Nothing Nothing []

addErr :: Int -> String -> State -> State
addErr ln msg st = st {errs = printf "L%d: %s" ln msg : errs st}

bump :: Char -> Char
bump c = C.chr (C.ord c + 1)

step :: Int -> String -> State -> State
step ln line st =
  case parseLine line of
    PartHdr ->
      st {curPart = Nothing, expLetter = Just 'a', expRoman = Nothing, expNum = Nothing, parent = Nothing}
    Top p l ->
      let st1 = case curPart st of
            Nothing -> st {curPart = Just p}
            Just cp -> if cp /= p then addErr ln (printf "part mismatch: saw %d expected %d" p cp) st else st
          expL = maybe 'a' id (expLetter st1)
          st2 =
            if l /= expL
              then addErr ln (printf "expected **%d%c.** but found **%d%c.**" (maybe p id (curPart st1)) expL p l) st1
              else st1
       in st2
            { expLetter = Just (bump l),
              expRoman = Just "i",
              expNum = Just 1,
              parent = Just (maybe p id (curPart st2), l)
            }
    Sub p l r ->
      let st1 = case parent st of
            Nothing -> addErr ln "subclause before parent" st
            Just (pp, ll) -> if pp /= p || ll /= l then addErr ln (printf "parent mismatch %d%c expected %d%c" p l pp ll) st else st
          exr = maybe "i" id (expRoman st1)
          st2 =
            if romanToInt r /= romanToInt exr
              then addErr ln (printf "expected roman .%s. but found .%s." exr r) st1
              else st1
       in st2 {expRoman = Just (intToRoman (romanToInt r + 1)), expNum = Just 1}
    SubSub p l r n ->
      let st1 = case parent st of
            Nothing -> addErr ln "sub-subclause before parent" st
            Just (pp, ll) -> if pp /= p || ll /= l then addErr ln (printf "parent mismatch %d%c expected %d%c" p l pp ll) st else st
          en = maybe 1 id (expNum st1)
          st2 =
            if n /= en
              then addErr ln (printf "expected .%d. after %d%c.%s but found .%d." en p l r n) st1
              else st1
       in st2 {expNum = Just (n + 1)}
    Other -> st

main :: IO ()
main = do
  args <- getArgs
  let path = if null args then "constitution.md" else head args
  ok <- doesFileExist path
  if not ok
    then putStrLn ("File not found: " ++ path) >> exitFailure
    else do
      content <- readFile path
      let final = L.foldl' (\s (i, l) -> step i l s) emptyS (zip [1 ..] (lines content))
      case errs final of
        [] -> putStrLn "Numbering validation passed" >> exitSuccess
        es -> do
          putStrLn "Numbering validation failed:\n"
          mapM_ putStrLn (reverse es)
          exitFailure
