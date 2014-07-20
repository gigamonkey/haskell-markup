module Markup
    (Markup(Document,
            Header,
            Paragraph,
            Section,
            Tagged,
            Text,
            Verbatim,
            Blockquote,
            UnorderedList,
            OrderedList,
            Item,
            Linkdef,
            Link),
     document) where

import Control.Monad
import Text.Parsec hiding (newline)

-- Document representation ---------------------------------------------

data Markup = Document [Markup]
            | Header Int [Markup]
            | Paragraph [Markup]
            | Section String [Markup]
            | Tagged String [Markup]
            | Text String
            | Verbatim String
            | Blockquote [Markup]
            | OrderedList [Markup]
            | UnorderedList [Markup]
            | Item [Markup]
            | Linkdef String String
            | Link String (Maybe String)
            deriving (Show, Eq)

-- Main elements -------------------------------------------------------

document :: Stream s m Char => ParsecT s (Int, Int, Int) m Markup
document = do
  optional modeline
  many (try eol)
  paragraphs <- many element
  eod
  return (Document paragraphs)

element = indentation >> (
                          header <|>
                          section <|>
                          verbatim <|>
                          unorderedList <|>
                          orderedList <|>
                          blockquote <|>
                          linkdef <|>
                          paragraph
                         )

header = do
  level <- headerMarker
  text  <- paragraphText
  return (Header level text)
  <?> "header"

section = do
  name       <- sectionMarker
  paragraphs <- many sectionBody
  sectionEnd
  blank
  return (Section name paragraphs)
  <?> "section"

sectionEnd = string "##."

verbatim      = indented 3 (liftM Verbatim verbatimText) <?> "verbatim"

blockquote    = indented 2 (liftM Blockquote (many1 element)) <?> "blockquote"

paragraph     = liftM Paragraph paragraphText <?> "paragraph"

unorderedList = list UnorderedList '-' <?> "unordered list"

orderedList   = list OrderedList '#' <?> "ordered list"

linkdef = do
  char '['
  name <- many (noneOf "]")
  string "] <"
  link <- many (noneOf ">")
  char '>'
  blank
  return (Linkdef name link)

modeline = do
  try (string "-*-")
  many ((notFollowedBy blank) >> anyChar)
  blank

-- And the nitty gritty details ----------------------------------------

headerMarker = do
  stars <- many1 (char '*')
  whitespace
  return (length stars)

verbatimText = do
  lines <- many1 (verbatimLine <|> verbatimBlankLine)
  return (concat $ dropTrailingBlanks lines)
      where dropTrailingBlanks lines = reverse $ (dropLastNewline (dropWhile ("\n" ==) $ reverse lines))
            dropLastNewline lines =
                case lines of
                  head:tail -> (reverse (dropWhile ('\n' ==) $ reverse head)) : tail
                  [] -> lines

verbatimLine = do
  indentation
  text <- many1 (notFollowedBy eol >> anyChar)
  eol <|> try eod
  return (text ++ "\n")

verbatimBlankLine = try eol >> return "\n" <?> "verbatim blank"

paragraphText = do
  text <- many1 (textUntil ((void tagOpen) <|> (void (char '['))) <|> taggedText <|> link)
  blank
  return text

link = do
  char '['
  link <- many1 (noneOf "|]")
  maybeKey <- optionMaybe linkKey
  char ']'
  return (Link link maybeKey)

linkKey = char '|' >> many1 (noneOf "]")

textUntil p = liftM Text $ many1 $ charsUntil p

charsUntil p = notFollowedBy p >> (plainChar <|> newlineChar <|> escapedChar)

plainChar = inSubdoc (noneOf "\\\n}") (noneOf "\\\n")

newlineChar = notFollowedBy blank >> newline >> indentation >> return ' '

escapedChar = char '\\' >> oneOf "\\{}*#-[]"

taggedText = do
  name <- tagOpen
  text <- if name == "note" then do subdocContents else do simpleContents
  char '}'
  return (Tagged name text)

simpleContents = many1 (textUntil taggedOrBrace <|> taggedText)

subdocContents = do
  (a, b, subdocLevel) <- getState
  setState (a, b, subdocLevel + 1)
  paragraphs <- many1 element
  eod
  setState (a, b, subdocLevel - 1)
  return paragraphs

tagOpen = do
  notFollowedBy escapedChar
  char '\\'
  name <- name
  char '{'
  return name

taggedOrBrace = void tagOpen <|> void (char '}')

sectionMarker = do
  string "## "
  n <- name
  many eol
  return n

sectionBody = notFollowedBy sectionEnd >> element

name = many1 letter

list c m = liftM c $ indented 2 $ many1 (try (indentation >> listElement m))

listElement m = do
  try (char m >> char ' ')
  extraIndentation 2
  contents <- many1 (indentation >> (orderedList <|> unorderedList <|> paragraph))
  dedent 2
  return (Item contents)


-- Whitespace and indentation handling ---------------------------------

whitespace = many (oneOf " \t") <?> "whitespace"

eol = whitespace >> newline <?> "end of line"

eod = inSubdoc braceAsEod eof

blank = do
  eol <|> try eod
  eol <|> eod
  return ()
  <?> "blank line"

indented n p = do
  indent n
  try (lookAhead (string (replicate n ' ')))
  r <- p
  dedent n
  return r

indentation = do
  (current, soFar, subdocLevel) <- getState
  let i = current - soFar
  try (string (replicate i ' '))
  setState (current, current, subdocLevel)

indent n = do
  (orig, soFar, subdocLevel) <- getState
  setState (orig + n, soFar, subdocLevel)

dedent n = do
  (orig, soFar, subdocLevel) <- getState
  setState (orig - n, soFar, subdocLevel)

extraIndentation n = do
  (i, _, subdocLevel) <- getState
  setState (i + n, i + n, subdocLevel)

newline = do
  (i, _, subdocLevel) <- getState
  setState (i, 0, subdocLevel)
  void (char '\n')
  <?> "newline"

braceAsEod = void $ lookAhead $ char '}'

inSubdoc p1 p2 = do
  subdocLevel <- getSubdocLevel
  if subdocLevel > 0 then do p1 else do p2

getSubdocLevel = do
  (_, _, subdocLevel) <- getState
  return subdocLevel
