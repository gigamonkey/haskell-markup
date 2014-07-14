module Markup where

import Debug.Trace
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec

-- Document representation ---------------------------------------------

data Markup = Document [Markup]
            | Header Int [Markup]
            | Paragraph [Markup]
            | Section String [Markup]
            | Tagged String [Markup]
            | Text String
            | Verbatim String
            deriving (Show, Eq)


-- Parser --------------------------------------------------------------

document :: GenParser Char Int Markup
document = do
  many (try eol)
  paragraphs <- many documentElement
  eof
  return (Document paragraphs)

documentElement = header <|> section <|> verbatim <|> paragraph

header = do
  indentation
  level <- headerMarker
  text  <- paragraphText
  return (Header level text)
  <?> "header"

headerMarker = do
  stars <- many1 (char '*')
  whitespace
  return (length stars)

paragraph = do
  indentation
  text <- paragraphText
  return (Paragraph text)
  <?> "paragraph"

verbatim = do
  indent 3
  lines <- many1 (verbatimLine <|> verbatimBlankLine)
  dedent 3
  return (Verbatim $ concat $ dropTrailingBlanks lines)
    where dropTrailingBlanks lines =
            reverse $ dropWhile ("\n" ==) $ reverse lines

indent n = do
  current <- getState
  setState (current + n)

dedent n = do
  current <- getState
  setState (current - n)

indentation = do
  i <- getState
  string (replicate i ' ')

verbatimLine = do
  indentation
  text <- many1 (notFollowedBy eol >> anyChar)
  eol <|> try eof
  return (text ++ "\n")

verbatimBlankLine = eol >> return "\n"

paragraphText = do
  text <- many1 ((textUntil taggedText) <|> taggedText)
  blank
  return text

textUntil p = many1 (charsUntil p) >>= return . Text

charsUntil p = do
  notFollowedBy p
  plainChar <|> singleNL <|> escapedChar

taggedOrBrace = (taggedText >> return ()) <|> (char '}' >> return ())

plainChar = noneOf "\\\n"

escapedChar = char '\\' >> oneOf "\\{}*#"

taggedText = do
  notFollowedBy escapedChar
  char '\\'
  name <- name
  char '{'
  text <- many1 ((textUntil taggedOrBrace) <|> taggedText)
  char '}'
  return (Tagged name text)

section = do
  indentation
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
