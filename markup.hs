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
  many eol
  paragraphs <- many documentElement
  eof
  return (Document paragraphs)

documentElement = header <|> section <|> paragraph

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
  name       <- sectionMarker
  paragraphs <- many sectionBody
  string "#."
  blank
  return (Section name paragraphs)

sectionMarker = do
  string "# "
  n <- name
  many eol
  return n

sectionBody = do
  notFollowedBy (string "#.")
  documentElement

name = many1 letter

paragraphText = do
  text <- many1 (noneOf "\n" <|> singleNL)
  blank
  return text

singleNL = do
  notFollowedBy blank
  char '\n'
  return ' '

eol = do
  whitespace
  char '\n'
  return ()
  <?> "end of line"

blank = do
  eol <|> try eof
  eol <|> eof
  return ()
  <?> "blank line"

whitespace = many (char ' ' <|> char '\t')

emptyDoc    = Document []
fooDoc      = Document [Paragraph "foo"]
fooBarDoc   = Document [Paragraph "foo bar"]
helloDoc    = Document [Paragraph "hello, world! goodbye!", Paragraph "Blah blah blah."]
headersDoc  = Document [Header 1 "foo", Paragraph "paragraph", Header 2 "bar"]
sectionDoc  = Document [ Section "foo" [Paragraph "bar", Paragraph "baz"] ]
sectionDoc2 = Document [ Section "foo" [Header 1 "bar", Paragraph "baz"] ]

shouldParse =
  [ ("", emptyDoc)
  , ("\n", emptyDoc)
  , ("\n\n", emptyDoc)
  , ("\n\n\n", emptyDoc)
  , ("\n\n\n\n", emptyDoc)
  , ("\n  \n\n\n", emptyDoc)
  , ("\n\n  \n\n", emptyDoc)
  , ("foo", fooDoc)
  , ("foo bar", fooBarDoc)
  , ("foo\nbar", fooBarDoc)
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
  , ("# foo\n\nbar\n\nbaz\n\n#.\n", sectionDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.", sectionDoc)
  , ("# foo\n\n* bar\n\nbaz\n\n#.\n\n", sectionDoc2)
  , ("# foo\n\n* bar\n\nbaz\n\n#.  \n\n", sectionDoc2)
  ]


homoiconic s =
  concat (["\""] ++ (map hc s) ++ ["\""])
  where hc c = case c of
          '\n' -> "\\n"
          '\t' -> "\\t"
          '"'  -> "\\\""
          _    -> [c]


main = forM_ shouldParse $ \t ->
  putStrLn $ case check t of
    (True, _)    -> "pass: (input: " ++ homoiconic (fst t) ++ ")"
    (False, msg) -> "\nFAIL: (input: " ++ homoiconic (fst t) ++ ") " ++ msg ++ "\n"

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
