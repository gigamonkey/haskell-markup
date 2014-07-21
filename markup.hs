module Markup
    (Markup(Document,
            Header,
            Paragraph,
            Section,
            SectionDivider,
            Tagged,
            Text,
            Verbatim,
            Blockquote,
            OrderedList,
            UnorderedList,
            DefinitionList,
            Item,
            Term,
            Definition,
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
            | SectionDivider
            | Tagged String [Markup]
            | Text String
            | Verbatim String
            | Blockquote [Markup]
            | OrderedList [Markup]
            | UnorderedList [Markup]
            | DefinitionList [Markup]
            | Term [Markup]
            | Definition [Markup]
            | Item [Markup]
            | Linkdef String String
            | Link [Markup] (Maybe String)
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
                          orderedList <|>
                          unorderedList <|>
                          definitionList <|>
                          blockquote <|>
                          (try linkdef) <|>
                          sectionDivider <|>
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

orderedList   = list OrderedList '#' <?> "ordered list"

unorderedList = list UnorderedList '-' <?> "unordered list"

sectionDivider = do
  char '§'
  blank
  return SectionDivider

definitionList = indented 2 (lookAhead term >> (liftM DefinitionList (many1 (term <|> definition)))) <?> "definition list"

term = do
  try (indentation >> string "% ")
  term <- many1 (textUntil taggedOrPercent <|> taggedText)
  string " %"
  eol
  return (Term term)

definition = liftM Definition $ many1 definitionP

definitionP = do
  try (indentation >> notFollowedBy (string "% "))
  paragraph


linkdef = do
  char '['
  name <- many1 (charsUntil (char ']'))
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
  lines <- many1 (verbatimBlankLine <|> verbatimLine)
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
  text <- many1 (textUntil (void tagOpen <|> void (char '[') <|> blank) <|> taggedText <|> link)
  blank
  return text

link = do
  char '['
  contents <- linkContents
  maybeKey <- optionMaybe linkKey
  char ']'
  return (Link contents maybeKey)

linkContents = many1 (textUntil taggedOrBracket <|> taggedText)

linkKey = char '|' >> many1 (noneOf "]")

textUntil p = liftM Text $ many1 $ charsUntil p

charsUntil p = notFollowedBy p >> (escapedChar <|> newlineChar <|> plainChar)

plainChar = inSubdoc (noneOf "}") anyChar

newlineChar = notFollowedBy blank >> newline >> indentation >> return ' '

escapedChar = try (char '\\' >> oneOf "\\{}*#-[]%|<") <?> "escaped char"

taggedText = do
  name <- tagOpen
  text <- if (name == "note" || name == "comment") then do subdocContents else do simpleContents
  char '}'
  return (Tagged name text)

simpleContents = many1 (textUntil taggedOrBrace <|> taggedText)

subdocContents = do
  (a, b, subdocLevel) <- getState
  setState (a, b, subdocLevel + 1)
  paragraphs <- many1 element
  eod
  setState (a, b, subdocLevel)
  return paragraphs

tagOpen = do
  notFollowedBy escapedChar
  char '\\'
  name <- name
  char '{'
  return name
  <?> "tagOpen"

taggedOrBrace = void tagOpen <|> void (char '}')

taggedOrBracket = void tagOpen <|> void (oneOf "|]")

taggedOrPercent = void tagOpen <|> void (string " %")

sectionMarker = do
  string "## "
  n <- name
  many1 (try eol)
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

blank =  do
  eol <|> eod
  void (many1 (try eol)) <|> eod
  return ()
  <?> "blank"

indented n p = do
  indent n
  try (lookAhead (count n (char ' ')))
  r <- p
  dedent n
  return r

indentation = do
  (current, soFar, subdocLevel) <- getState
  let i = current - soFar
  try $ count i (char ' ')
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
