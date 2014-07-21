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
     markup) where

import Control.Monad
import Text.Parsec hiding (newline)
import Data.Set (fromList, member)

-- Main API ------------------------------------------------------------

markup subdocs filename text = runParser document (0, 0, 0, fromList subdocs) filename (concatMap detab text)
    where
      detab '\t' = replicate 8 ' '
      detab c    = [c]

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

document = do
  optional modeline
  many (try eol)
  paragraphs <- many element
  eod
  return (Document paragraphs)

element = indentation >> anElement
    where anElement = header         <|>
                      section        <|>
                      verbatim       <|>
                      orderedList    <|>
                      unorderedList  <|>
                      definitionList <|>
                      blockquote     <|>
                      try linkdef    <|>
                      sectionDivider <|>
                      paragraph

header = do
  level <- headerMarker
  text  <- paragraphText
  return (Header level text)
  <?> "header"
      where headerMarker = do
              stars <- many1 (char '*')
              whitespace
              return (length stars)

section = do
  name       <- sectionMarker
  paragraphs <- many sectionBody
  sectionEnd
  blank
  return (Section name paragraphs)
  <?> "section"
      where
        sectionMarker = do
            string "## "
            n <- name
            many1 (try eol)
            return n

        sectionEnd = string "##."

        sectionBody = notFollowedBy sectionEnd >> element

verbatim = indented 3 (liftM Verbatim verbatimText) <?> "verbatim"
    where
      verbatimText = do
        lines <- many1 (verbatimBlankLine <|> verbatimLine)
        return (concat $ dropTrailingBlanks lines)

      verbatimBlankLine =
          try eol >> return "\n" <?> "verbatim blank"

      verbatimLine = do
        indentation
        text <- many1 (notFollowedBy eol >> anyChar)
        eol <|> try eod
        return (text ++ "\n")

      dropTrailingBlanks lines =
          reverse $ dropLastNewline (dropWhile ("\n" ==) $ reverse lines)

      dropLastNewline lines =
          case lines of
            head:tail -> reverse (dropWhile ('\n' ==) $ reverse head) : tail
            [] -> lines

orderedList = list OrderedList '#' <?> "ordered list"

unorderedList = list UnorderedList '-' <?> "unordered list"

definitionList = indented 2 (lookAhead term >> liftM DefinitionList (many1 (term <|> definition))) <?> "definition list"
    where term = do
            try (indentation >> string "% ")
            term <- many1 (textUntil (taggedOr (string " %")) <|> taggedText)
            string " %"
            eol
            return (Term term)
          definition = liftM Definition $ many1 definitionP
          definitionP = do
                   try (indentation >> notFollowedBy (string "% "))
                   paragraph

blockquote = indented 2 (liftM Blockquote (many1 blockquoteElement)) <?> "blockquote"
    where blockquoteElement = do
            try (notFollowedBy (count 3 (char ' ') >> noneOf " "))
            element

linkdef = do
  name <- linkdefName
  char ' '
  link <- linkdefLink
  blank
  return (Linkdef name link)
      where linkdefName = between (char '[') (char ']') (charsUntil (char ']'))
            linkdefLink = between (char '<') (char '>') (many (noneOf ">"))

sectionDivider = whitespace >> char '§' >> blank >> return SectionDivider

paragraph = liftM Paragraph paragraphText <?> "paragraph"

-- And the nitty gritty details ----------------------------------------

modeline = do
  try (string "-*-")
  many (notFollowedBy blank >> anyChar)
  blank

paragraphText = do
  text <- many1 (textUntil (taggedOr (char '[') <|> blank) <|> taggedText <|> linkref)
  blank
  return text

linkref = do
  char '['
  contents <- linkContents
  maybeKey <- optionMaybe linkKey
  char ']'
  return (Link contents maybeKey)
    where
      linkContents = many1 (textUntil (taggedOr (oneOf "|]")) <|> taggedText)
      linkKey      = char '|' >> many1 (noneOf "]")

escapedChar = try (char '\\' >> oneOf "\\{}*#-[]%|<") <?> "escaped char"

newlineChar = notFollowedBy blank >> newline >> indentation >> return ' '

plainChar = inSubdoc (noneOf "}") anyChar

taggedText = do
  (_, _, _, subdocs) <- getState
  name <- tagOpen
  text <- if name `member` subdocs then subdocContents else simpleContents
  char '}'
  return (Tagged name text)

tagOpen = do
  notFollowedBy escapedChar
  char '\\'
  name <- name
  char '{'
  return name
  <?> "tagOpen"

subdocContents = do
  (a, b, subdocLevel, sds) <- getState
  setState (a, b, subdocLevel + 1, sds)
  paragraphs <- many1 element
  eod
  setState (a, b, subdocLevel, sds)
  return paragraphs

simpleContents = many1 (textUntil (taggedOr $ char '}') <|> taggedText)

taggedOr p = void tagOpen <|> void p

name = many1 letter

list c m = liftM c $ indented 2 $ many1 (try (indentation >> listElement m))
    where
      listElement m = do
        try (char m >> char ' ')
        extraIndentation 2
        contents <- many1 (indentation >> (orderedList <|> unorderedList <|> paragraph))
        dedent 2
        return (Item contents)

-- Whitespace and indentation handling ---------------------------------

whitespace = many (oneOf " \t") <?> "whitespace"

eol = whitespace >> newline <?> "end of line"

eod = inSubdoc endOfSubdoc eof
    where endOfSubdoc = void (lookAhead (char '}'))

blank =  do
  eol <|> eod
  void (many1 (try eol)) <|> eod
  <?> "blank"

indentation = do
  (current, soFar, subdocLevel, sds) <- getState
  let i = current - soFar
  try $ count i (char ' ')
  setState (current, current, subdocLevel, sds)

indent n = do
  (orig, soFar, subdocLevel, sds) <- getState
  setState (orig + n, soFar, subdocLevel, sds)

dedent n = do
  (orig, soFar, subdocLevel, sds) <- getState
  setState (orig - n, soFar, subdocLevel, sds)

extraIndentation n = do
  (i, _, subdocLevel, sds) <- getState
  setState (i + n, i + n, subdocLevel, sds)

newline = do
  (i, _, subdocLevel, sds) <- getState
  setState (i, 0, subdocLevel, sds)
  void (char '\n')
  <?> "newline"

-- Combinators ---------------------------------------------------------

textUntil p = liftM Text $ charsUntil p

charsUntil p = many1 $ notFollowedBy p >> (escapedChar <|> newlineChar <|> plainChar)

indented n p = do
  indent n
  try (lookAhead (count n (char ' ')))
  r <- p
  dedent n
  return r

inSubdoc p1 p2 = do
  (_, _, subdocLevel, _) <- getState
  if subdocLevel > 0 then p1 else p2
