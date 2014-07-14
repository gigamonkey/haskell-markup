module Markup where

import Debug.Trace
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec hiding (newline)

-- Document representation ---------------------------------------------

data Markup = Document [Markup]
            | Header Int [Markup]
            | Paragraph [Markup]
            | Section String [Markup]
            | Tagged String [Markup]
            | Text String
            | Verbatim String
            | Blockquote [Markup]
            deriving (Show, Eq)


-- Parser --------------------------------------------------------------

document :: GenParser Char (Int, Int) Markup
document = do
  many (try eol)
  paragraphs <- many element
  eof
  return (Document paragraphs)

element = indentation >> (header <|> section <|> verbatim <|> blockquote <|> paragraph)

header = do
  level <- headerMarker
  text  <- paragraphText
  return (Header level text)
  <?> "header"

section = do
  name       <- sectionMarker
  paragraphs <- many sectionBody
  string "#."
  blank
  return (Section name paragraphs)

paragraph  = liftM Paragraph paragraphText <?> "paragraph"

blockquote = indented 2 (liftM Blockquote (many1 element)) <?> "blockquote"

verbatim   = indented 3 (liftM Verbatim verbatimText) <?> "verbatim"

headerMarker = do
  stars <- many1 (char '*')
  whitespace
  return (length stars)

verbatimText = do
  lines <- many1 (verbatimLine <|> verbatimBlankLine)
  return (concat $ dropTrailingBlanks lines)
      where dropTrailingBlanks lines = reverse $ dropWhile ("\n" ==) $ reverse lines

verbatimLine = do
  indentation
  text <- many1 (notFollowedBy eol >> anyChar)
  eol <|> try eof
  return (text ++ "\n")

verbatimBlankLine = try eol >> return "\n" <?> "verbatim blank"

paragraphText = do
  text <- many1 (textUntil taggedText <|> taggedText)
  blank
  return text

textUntil p = liftM Text $ many1 $ charsUntil p

charsUntil p = notFollowedBy p >> (plainChar <|> newlineChar <|> escapedChar)

plainChar = noneOf "\\\n"

newlineChar = notFollowedBy blank >> newline >> indentation >> return ' '

escapedChar = char '\\' >> oneOf "\\{}*#"

taggedText = do
  notFollowedBy escapedChar
  char '\\'
  name <- name
  char '{'
  text <- many1 (textUntil taggedOrBrace <|> taggedText)
  char '}'
  return (Tagged name text)

taggedOrBrace = void taggedText <|> void (char '}')

sectionMarker = do
  string "# "
  n <- name
  many eol
  return n

sectionBody = notFollowedBy (string "#.") >> element

name = many1 letter

eol = whitespace >> newline <?> "end of line"

newline = do
  char '\n'
  (i, soFar) <- getState
  setState (i, 0)
  return ()
  <?> "newline"

blank = do
  eol <|> try eof
  eol <|> eof
  return ()
  <?> "blank line"

whitespace = many (oneOf " \t") <?> "whitespace"

indented n p = do
  (orig, soFar) <- getState
  let new = orig + n
  setState (new, soFar)
  try (lookAhead (string (replicate n ' ')))
  r <- p
  (current, soFar) <- getState
  setState (orig, soFar)
  return r

indentation = do
  (current, soFar) <- getState
  let i = current - soFar
  try (string (replicate i ' '))
  setState (current, current)
