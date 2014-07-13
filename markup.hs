import Debug.Trace
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec

data Markup = Document [Markup]
            | Header Int [Markup]
            | Paragraph [Markup]
            | Section String [Markup]
            | Tagged String [Markup]
            | Text String
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

paragraphText = do
  text <- many1 (plainText <|> taggedText)
  blank
  return text

plainText = do
  text <- many1 plainTextChar
  return (Text text)

plainTextChar = do
  notFollowedBy taggedText
  plainChar <|> singleNL <|> escapedChar

inTaggedPlainText =  do
  text <- many1 inTaggedPlainTextChar
  return (Text text)


inTaggedPlainTextChar = do
  notFollowedBy taggedText
  notFollowedBy (char '}')
  plainChar <|> singleNL <|> escapedChar

plainChar = noneOf "\\\n"

escapedChar = do
  char '\\'
  oneOf "\\{*#"

taggedText = do
  notFollowedBy escapedChar
  char '\\'
  name <- name
  char '{'
  text <- many1 (inTaggedPlainText <|> taggedText)
  char '}'
  return (Tagged name text)

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

d = Document
p = Paragraph
t = Text
h = Header
s = Section
i = Tagged "i"
b = Tagged "b"

emptyDoc    = d []
fooDoc      = d [ p [ t "foo"]]
fooBarDoc   = d [ p [ t "foo bar"]]
helloDoc    = d [ p [ t "hello, world! goodbye!"], p [t "Blah blah blah."]]
headersDoc  = d [ h 1 [ t "foo"], p [ t "paragraph"], h 2 [t "bar"]]
sectionDoc  = d [ s "foo" [ p [ t "bar"], p [t "baz"]]]
sectionDoc2 = d [ s "foo" [ h 1 [t "bar"], p [ t "baz"]]]

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
  , ("foo\\{\n\n", d [p [t "foo{"]])
  , ("foo\\\\\n\n", d [p [t "foo\\"]])
  , ("foo\\*\n\n", d [p [t "foo*"]])
  , ("foo\\#\n\n", d [p [t "foo#"]])
  , ("foo \\i{bar} baz", d [p [t "foo ", i [t "bar"], t " baz"]])
  , ("foo \\i{\\b{bar}} baz", d [p [t "foo ", i [ b [t "bar"]], t " baz"]])
  , ("foo \\i{foo \\b{bar} baz} baz", d [p [t "foo ", i [ t "foo ", b [t "bar"], t " baz"], t " baz"]])
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n\n", helloDoc)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n", helloDoc)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.", helloDoc)
  , ("* foo", d [h 1 [t "foo"]])
  , ("* foo\n\n** bar", d [h 1 [t "foo"], h 2 [t "bar"]])
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
          '\\' -> "\\\\"
          '"'  -> "\\\""
          _    -> [c]


main = forM_ shouldParse $ \t ->
  putStrLn $ case check t of
    (True, _)    -> "ok: " ++ homoiconic (fst t) ++ ""
    (False, msg) -> "\nFAIL: " ++ homoiconic (fst t) ++ ":\n" ++ msg ++ "\n"

testParse :: String -> Either ParseError Markup
testParse = parse document "(unknown)"

check (input, expected) =
  case testParse input of
    Left err -> (False, show err)
    Right d ->
      if d == expected then
        (True, "")
      else
        (False, "  Got: " ++ show d ++ "\n  Exp: " ++ show expected)
