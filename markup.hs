import Debug.Trace
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec

data Markup = Document [Markup]
            | Header Int String
            | Paragraph String
            | Section String [Markup]
            deriving (Show, Eq)

document :: GenParser Char st Markup
document = do
  many blank
  paragraphs <- many (header <|> section <|> paragraph)
  eof
  return (Document paragraphs)

header = do
  level <- headerMarker
  text  <- paragraphText
  return (Header level text)
  <?> "header"

headerMarker = do
  stars <- many1 (char '*')
  many1 (char ' ')
  return (length stars)

paragraph = do
  text <- paragraphText
  return (Paragraph text)
  <?> "paragraph"

section = do
  string "# "
  n <- name
  eol
  blank
  paragraphs <- many sectionBody
  string "#."
  eol
  blank
  return (Section n paragraphs)

sectionBody = do
  notFollowedBy (string "#.")
  (header <|> section <|> paragraph)

name = many1 letter

paragraphText = do
  text <- endBy1 lineText (eol <|> try eof)
  blank <|> eof
  return (unwords text)

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

emptyDoc   = Document []
fooDoc     = Document [Paragraph "foo"]
helloDoc   = Document [Paragraph "hello, world! goodbye!", Paragraph "Blah blah blah."]
headersDoc = Document [Header 1 "foo", Paragraph "paragraph", Header 2 "bar"]
sectionDoc = Document [ Section "foo" [Paragraph "bar", Paragraph "baz"] ]

shouldParse =
  [ ("", emptyDoc)
  , ("\n", emptyDoc)
  , ("\n\n", emptyDoc)
  , ("\n\n\n", emptyDoc)
  , ("\n\n\n\n", emptyDoc)
  , ("\n  \n\n\n", emptyDoc)
  , ("\n\n  \n\n", emptyDoc)
  , ("foo", fooDoc)
  , ("foo\n", fooDoc)
  , ("foo\n\n", fooDoc)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n\n", helloDoc)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n", helloDoc)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.", helloDoc)
  , ("* foo", Document [Header 1 "foo"])
  , ("* foo\n\n** bar", Document [Header 1 "foo", Header 2 "bar"])
  , ("* foo\n\nparagraph\n  \t \n** bar", headersDoc)
  , ("*    foo\n\nparagraph\n  \t \n** bar", headersDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.\n\n", sectionDoc)
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
