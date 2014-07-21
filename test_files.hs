import Control.Monad
import Data.Aeson
import Markup
import System.Environment
import Text.Parsec

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Vector as V

data Result = BadJson B.ByteString
            | BadParse ParseError String
            | Mismatch Value Value Markup
            | Okay


-- Markup to Json ------------------------------------------------------

jsonify :: Markup -> Value
jsonify (Document ms)                 = tagged "body" ms
jsonify (Header i ms)                 = tagged ("h" ++ show i) ms
jsonify (Paragraph ms)                = tagged "p" ms
jsonify (Section tag ms)              = tagged tag ms
jsonify (Tagged tag ms)               = tagged tag ms
jsonify (Blockquote ms)               = tagged "blockquote" ms
jsonify (OrderedList ms)              = tagged "ol" ms
jsonify (UnorderedList ms)            = tagged "ul" ms
jsonify (Item ms)                     = tagged "li" ms
jsonify (Text s)                      = text s
jsonify (Verbatim s)                  = taggedText "pre" s
jsonify (Linkdef n l)                 = tagged2 "link_def" [taggedText "link" n, taggedText "url" l]
jsonify (Link (Text n : []) Nothing)  = taggedText "link" n
jsonify (Link ms Nothing)             = tagged "link" ms
jsonify (Link (Text n : []) (Just k)) = tagged2 "link" [text n, taggedText "key" k]
jsonify (Link ms (Just k))            = tagged2 "link" (map jsonify ms ++ [taggedText "key" k])
jsonify (DefinitionList ms)           = tagged "dl" ms
jsonify (Term ms)                     = tagged "dt" ms
jsonify (Definition ms)               = tagged "dd" ms
jsonify SectionDivider                = taggedText "section" "§"

tagged tag ms = Array (V.fromList $ text tag : map jsonify ms)
text t        = String $ T.pack t

taggedText tag t = tagged2 tag [text t]
tagged2 tag xs = Array (V.fromList $ text tag : xs)



-- Parse test files ----------------------------------------------------

compareParses a bytes text =
  case decode bytes of
    Just j ->
      case markup a text of
        Right m -> if j == jsonify m then Okay else Mismatch j (jsonify m) m
        Left e  -> BadParse e text
    Nothing -> BadJson bytes

checkFile reporter summary a = do
  bytes  <- B.readFile $ take (length a - length ".txt") a ++ ".json"
  markup <- readFile a
  let result = compareParses a bytes markup
  reporter a result
  return (summarize summary result)

summarize (bj, bp, m, ok) (BadJson {})    = (bj + 1, bp, m, ok)
summarize (bj, bp, m, ok) (BadParse {}) = (bj, bp + 1, m, ok)
summarize (bj, bp, m, ok) (Mismatch {}) = (bj, bp, m + 1, ok)
summarize (bj, bp, m, ok) Okay           = (bj, bp, m, ok + 1)

report a (BadJson {})  = putStrLn $ "Whoops! (Bad json) .... " ++ a
report a (BadParse {}) = putStrLn $ "FAIL (Bad parse) ...... " ++ a
report a (Mismatch {}) = putStrLn $ "FAIL (Mismatch) ....... " ++ a
report _ Okay          = return ()

verboseReport a (BadJson bs)     = putStrLn $ "Bad JSON in " ++ a ++ "\n\n" ++ show bs
verboseReport a (BadParse e m)   = putStrLn $ "Parse error in " ++ a ++ "\n" ++ show e ++ "\n\n" ++ show m
verboseReport _ Okay             = putStr "."
verboseReport a (Mismatch e g _) = do
    putStrLn $ "Mismatch in " ++ a ++ "\n\n"
    putStrLn "-- Expected ------"
    print e
    putStrLn "------------------"
    putStrLn ""
    putStrLn "-- Got -----------"
    print g
    putStrLn "------------------"


showResults (bj, bp, m, ok) = do
  putStrLn "\n"
  putStrLn $ "Bad JSON   : " ++ show bj
  putStrLn $ "Bad Parses : " ++ show bp
  putStrLn $ "Mismatches : " ++ show m
  putStrLn $ "Okay       : " ++ show ok

reporter args = if length args == 1 then verboseReport else report

main = do
  args <- getArgs
  results <- foldM (checkFile (reporter args)) (0, 0, 0, 0) args
  unless (length args == 1) $ showResults results
