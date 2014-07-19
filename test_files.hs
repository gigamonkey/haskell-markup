import Control.Monad
import Data.Aeson
import Markup
import System.Environment
import Text.Parsec

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Vector as V

data Result = BadJson B.ByteString
            | BadParse ParseError
            | Mismatch Value Value
            | Okay


-- Markup to Json ------------------------------------------------------

jsonify :: Markup -> Value
jsonify (Document ms)      = tagged "body" ms
jsonify (Header i ms)      = tagged ("h" ++ show i) ms
jsonify (Paragraph ms)     = tagged "p" ms
jsonify (Section tag ms)   = tagged tag ms
jsonify (Tagged tag ms)    = tagged tag ms
jsonify (Blockquote ms)    = tagged "blockquote" ms
jsonify (OrderedList ms)   = tagged "ol" ms
jsonify (UnorderedList ms) = tagged "ul" ms
jsonify (Item ms)          = tagged "li" ms
jsonify (Text s)           = text s
jsonify (Verbatim s)       = Array (V.fromList $ [(text "pre"), (text s)])

tagged tag ms = Array (V.fromList $ (text tag) : (map jsonify ms))
text t        = String $ T.pack t


-- Parse test files ----------------------------------------------------

testParse :: String -> String -> Either ParseError Markup
testParse file = runParser document (0, 0, 0) file

compareParses a bytes markup = do
  case decode bytes of
    Just j -> do
      case (testParse a markup) of
        Right m -> if (j == (jsonify m)) then Okay else  (Mismatch j (jsonify m))
        Left e  -> BadParse e
    Nothing -> BadJson bytes

message (BadJson bs)   = "Bad JSON: " ++ (show bs)
message (BadParse e)   = "Markup parse error: " ++ (show e)
message (Mismatch e g) = "Whoops!\nExpected: " ++ (show e) ++ "\nGot: " ++ (show g)
message Okay           = "okay"

checkFile a = do
  bytes  <- B.readFile $ (take ((length a) - (length ".txt")) a) ++ ".json"
  markup <- readFile a
  putStr a
  putStrLn $ " ... " ++ (message (compareParses a bytes markup))

main = getArgs >>= mapM_ checkFile
