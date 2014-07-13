import Debug.Trace
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec

data Markup = Document [Markup]
            | Paragraph String
            | Header Int String
            deriving (Show, Eq)

document :: GenParser Char st Markup
document = do
  many blank
  paragraphs <- many (header <|> paragraph)
  eof
  return (Document paragraphs)

header :: GenParser Char st Markup
header = do
  stars <- many1 (char '*')
  char ' '
  text <- endBy1 lineText (eol <|> try eof)
  blank <|> eof
  return (Header (length stars) (unwords text))
  <?> "header"

paragraph :: GenParser Char st Markup
paragraph = do
  text <- endBy1 lineText (eol <|> try eof)
  blank <|> eof
  return (Paragraph (unwords text))
  <?> "paragraph"

eol = do
  whitespace
  char '\n'
  return ()
  <?> "end of line"

blank = do
  whitespace
  string "\n"
  return ()
  <?> "blank line"

whitespace = many (char ' ' <|> char '\t')

lineText :: GenParser Char st String
lineText = do
  notFollowedBy blank
  many1 (noneOf "\n")

doc1 = Document [Paragraph "hello, world! goodbye!", Paragraph "Blah blah blah."]

shouldParse = [
  ("", Document [])
  , ("\n", Document [])
  , ("\n\n", Document [])
  , ("\n\n\n", Document [])
  , ("\n\n\n\n", Document [])
  , ("\n  \n\n\n", Document [])
  , ("\n\n  \n\n", Document [])
  , ("foo", Document [Paragraph "foo"])
  , ("foo\n", Document [Paragraph "foo"])
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n\n", doc1)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n", doc1)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.", doc1)
  , ("* foo", Document [Header 1 "foo"])
  , ("* foo\n\n** bar", Document [Header 1 "foo", Header 2 "bar"])
  , ("* foo\n\nparagraph\n  \t \n** bar", Document [Header 1 "foo", Paragraph "paragraph", Header 2 "bar"])
  ]


main = forM_ shouldParse $ \t ->
  putStrLn $ case check t of
    (True, _)    -> "Pass."
    (False, msg) -> "FAIL: (input: " ++ fst t ++ ") " ++ msg

testParse :: String -> Either ParseError Markup
testParse = parse document "(unknown)"

check (input, expected) =
  case testParse input of
    Left err -> (False, show err)
    Right d ->
      if d == expected then
        (True, "")
      else
        (False, "Got: " ++ show d ++ "\nExpected " ++ show expected)
