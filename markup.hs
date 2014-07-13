import Debug.Trace
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec

data Markup = Document [Markup]
            | Paragraph String
            deriving (Show, Eq)

document :: GenParser Char st Markup
document = do
  many blank
  paragraphs <- many paragraph
  eof
  return (Document paragraphs)

paragraph :: GenParser Char st Markup
paragraph = do
  text <- endBy1 lineText (eol <|> try eof)
  blank <|> eof
  return (Paragraph (foldl (++) [] (intersperse " " text)))
  <?> "paragraph"

eol = do
  char '\n'
  return ()
  <?> "end of line"

blank = do
  string "\n"
  return ()
  <?> "blank line"

lineText :: GenParser Char st String
lineText = many1 (noneOf "\n")

doc1 = Document [Paragraph "hello, world! goodbye!", Paragraph "Blah blah blah."]

shouldParse = [
  ("", Document []),
  ("\n", Document []),
  ("foo", Document [Paragraph "foo"]),
  ("foo\n", Document [Paragraph "foo"]),
  ("hello, world!\ngoodbye!\n\nBlah blah blah.\n\n", doc1),
  ("hello, world!\ngoodbye!\n\nBlah blah blah.\n", doc1),
  ("hello, world!\ngoodbye!\n\nBlah blah blah.", doc1)
  ]


main = do
  forM_ shouldParse $ \t -> do
    putStrLn $ case check t of
      (True, _)    -> "Pass."
      (False, msg) -> "FAIL: (input: " ++ (fst t) ++ ") " ++ msg

testParse :: String -> Either ParseError Markup
testParse input = parse document "(unknown)" input

check (input, expected) =
  case testParse input of
    Left err -> (False, (show err))
    Right d ->
      if d == expected then
         (True, "")
      else
        (False, "Got: " ++ (show d) ++ "\nExpected " ++ (show expected))
