import Control.Monad
import Data.Aeson
import Markup
import System.Environment
import Text.Parsec

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Vector as V


-- Tests ---------------------------------------------------------------

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

testParse :: String -> String -> Either ParseError Markup
testParse file = runParser document (0, 0) file

okay b = if b then " ... okay" else " ... WHOOPS!"

main = do
  args <- getArgs
  forM_ args $ \a -> do
         bytes  <- B.readFile $ (take ((length a) - (length ".txt")) a) ++ ".json"
         markup <- readFile a
         case decode bytes of
           Just j -> do
                     putStr $ a
                     case (testParse a markup) of
                       Right m -> if (j /= (jsonify m)) then do
                                      putStrLn " ... Whoops!"
                                      putStrLn $ "JSON: " ++ (show j)
                                      putStrLn $ "Markup: " ++ (show (jsonify m))
                                  else do
                                      putStrLn " ... okay"
                       Left e  -> do
                             putStrLn $ " ... parsing error: " ++ (show e)
           Nothing -> putStrLn $ "Couldn't parse json in " ++ (show bytes)
