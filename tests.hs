import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import Markup


-- Tests ---------------------------------------------------------------

d = Document
p = Paragraph
t = Text
h = Header
s = Section
i = Tagged "i"
b = Tagged "b"
v = Verbatim
bq = Blockquote

emptyDoc    = d []
fooDoc      = d [ p [ t "foo"]]
fooBarDoc   = d [ p [ t "foo bar"]]
helloDoc    = d [ p [ t "hello, world! goodbye!"], p [t "Blah blah blah."]]
headersDoc  = d [ h 1 [ t "foo"], p [ t "paragraph"], h 2 [t "bar"]]
sectionDoc  = d [ s "foo" [ p [ t "bar"], p [t "baz"]]]
sectionDoc2 = d [ s "foo" [ h 1 [t "bar"], p [ t "baz"]]]
complexDoc1 = d [
  h 1 [t "header"],
  p [t "A paragraph"],
  s "foo" [
    h 1 [t "bar"],
    p [ t "baz"]],
  p [t "Another paragraph"]
  ]

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
  , ("foo {bar} baz", d [p [t "foo {bar} baz"]])
  , ("foo \\{bar\\} baz", d [p [t "foo {bar} baz"]])
  , ("foo \\i{bar} baz", d [p [t "foo ", i [t "bar"], t " baz"]])
  , ("foo \\i{bar\\}} baz", d [p [t "foo ", i [t "bar}"], t " baz"]])
  , ("foo \\i{\\b{bar}} baz", d [p [t "foo ", i [ b [t "bar"]], t " baz"]])
  , ("foo \\i{foo \\b{bar} baz} baz", d [p [t "foo ", i [ t "foo ", b [t "bar"], t " baz"], t " baz"]])
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n\n", helloDoc)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.\n", helloDoc)
  , ("hello, world!\ngoodbye!\n\nBlah blah blah.", helloDoc)
  , ("* foo", d [h 1 [t "foo"]])
  , ("\n* foo", d [h 1 [t "foo"]])
  , ("\n\n* foo", d [h 1 [t "foo"]])
  , ("\n\n\n* foo", d [h 1 [t "foo"]])
  , ("\n\n\n\n* foo", d [h 1 [t "foo"]])
  , ("* foo\n\n** bar", d [h 1 [t "foo"], h 2 [t "bar"]])
  , ("* foo\n\nparagraph\n  \t \n** bar", headersDoc)
  , ("*    foo\n\nparagraph\n  \t \n** bar", headersDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.\n\n", sectionDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.\n", sectionDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.", sectionDoc)
  , ("# foo\n\n* bar\n\nbaz\n\n#.\n\n", sectionDoc2)
  , ("# foo\n\n* bar\n\nbaz\n\n#.  \n\n", sectionDoc2)
  , ("* header\n\nA paragraph\n\n# foo\n\n* bar\n\nbaz\n\n#.\n\nAnother paragraph", complexDoc1)
  , ("* header\n\nA\nparagraph\n\n# foo\n\n* bar\n\nbaz\n\n#.\n\nAnother paragraph", complexDoc1)
  , ("   verbatim", d [ v "verbatim\n"])
  , ("   verbatim\n", d [ v "verbatim\n"])
  , ("   verbatim\n\n", d [ v "verbatim\n"])
  , ("   verbatim\n   verbatim2\n", d [ v "verbatim\nverbatim2\n"])
  , ("   verbatim\n\n   verbatim2\n", d [ v "verbatim\n\nverbatim2\n"])
  , ("   verbatim\n\n\n   verbatim2\n", d [ v "verbatim\n\n\nverbatim2\n"])
  , ("   verbatim\n\n\n", d [ v "verbatim\n"])
  , ("   verbatim\n\nfoo", d [ v "verbatim\n", p [ t "foo"]])
  , ("   verbatim\n\n\nfoo", d [ v "verbatim\n", p [ t "foo"]])
  , ("   line one\n    line two\n    line three", d [ v "line one\n line two\n line three\n"])
  , ("   line one\n     line two\n    line three", d [ v "line one\n  line two\n line three\n"])
  , ("regular\n\n   verbatim\n\nregular", d [ p [t "regular"], v "verbatim\n", p [t "regular"]])
  , ("regular\n\n  blockquote\n\nregular", d [ p [t "regular"], bq [p [t "blockquote"]], p [t "regular"]])
  , ("regular\n\n  blockquote\n  and more\n\nregular", d [ p [t "regular"], bq [p [t "blockquote and more"]], p [t "regular"]])
  , ("regular\n\n  blockquote\n  and more\n\n     verbatim\n\nregular", d [ p [t "regular"], bq [ p [t "blockquote and more"], v "verbatim\n" ], p [t "regular"]])
  , ("regular\n\n  blockquote\n  and more\n\n    indented blockquote\n\nregular", d [ p [t "regular"], bq [ p [t "blockquote and more"], bq [p [t "indented blockquote" ]]], p [t "regular"]])
  ]

homoiconic s =
  concat (["\""] ++ map hc s ++ ["\""])
  where hc c = case c of
          '\n' -> "\\n"
          '\t' -> "\\t"
          '\\' -> "\\\\"
          '"'  -> "\\\""
          _    -> [c]

main = forM_ shouldParse $ \t ->
  putStr $ case check t of
    (True, _)    -> "."
    (False, msg) -> "\nFAIL: " ++ homoiconic (fst t) ++ ":\n" ++ msg ++ "\n"

testParse :: String -> Either ParseError Markup
testParse = runParser document 0 "input string"

check (input, expected) =
  case testParse input of
    Left err -> (False, show err)
    Right d ->
      if d == expected then
        (True, "")
      else
        (False, "  Exp: " ++ show expected ++ "\n  Got: " ++ show d)
