import Control.Monad
import Text.Parsec
import Markup

-- Tests ---------------------------------------------------------------

d  = Document
p  = Paragraph
t  = Text
h  = Header
s  = Section
i  = Tagged "i"
b  = Tagged "b"
v  = Verbatim
bq = Blockquote
ul = UnorderedList
ol = OrderedList
li = Item

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
oneHeader = d [h 1 [t "foo"]]
twoHeaders = d [h 1 [t "foo"], h 2 [t "bar"]]
simpleVerbatim = d [ v "verbatim"]

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
  , ("* foo",         oneHeader)
  , ("\n* foo",       oneHeader)
  , ("\n\n* foo",     oneHeader)
  , ("\n\n\n* foo",   oneHeader)
  , ("\n\n\n\n* foo", oneHeader)
  , ("* foo\n\n** bar", twoHeaders)
  , ("* foo\n\nparagraph\n  \t \n** bar", headersDoc)
  , ("*    foo\n\nparagraph\n  \t \n** bar", headersDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.\n\n", sectionDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.\n", sectionDoc)
  , ("# foo\n\nbar\n\nbaz\n\n#.", sectionDoc)
  , ("# foo\n\n* bar\n\nbaz\n\n#.\n\n", sectionDoc2)
  , ("# foo\n\n* bar\n\nbaz\n\n#.  \n\n", sectionDoc2)
  , ("* header\n\nA paragraph\n\n# foo\n\n* bar\n\nbaz\n\n#.\n\nAnother paragraph", complexDoc1)
  , ("* header\n\nA\nparagraph\n\n# foo\n\n* bar\n\nbaz\n\n#.\n\nAnother paragraph", complexDoc1)
  , ("   verbatim",                                                             simpleVerbatim)
  , ("   verbatim\n",                                                           simpleVerbatim)
  , ("   verbatim\n\n",                                                         simpleVerbatim)
  , ("   verbatim\n   verbatim2\n",                                             d [ v "verbatim\nverbatim2"])
  , ("   verbatim\n\n   verbatim2\n",                                           d [ v "verbatim\n\nverbatim2"])
  , ("   verbatim\n\n\n   verbatim2\n",                                         d [ v "verbatim\n\n\nverbatim2"])
  , ("   verbatim\n\n\n",                                                       simpleVerbatim)
  , ("   verbatim\n\nfoo",                                                      verbatimThenParagraph)
  , ("   verbatim\n\n\nfoo",                                                    verbatimThenParagraph)
  , ("   line one\n    line two\n    line three",                               threeLineVerbatim)
  , ("   line one\n     line two\n    line three",                              d [ v "line one\n  line two\n line three"])
  , ("regular\n\n   verbatim\n\nregular",                                       d [ p [t "regular"], v "verbatim", p [t "regular"]])
  , ("regular\n\n  blockquote\n\nregular",                                      d [ p [t "regular"], bq [ p [t "blockquote"]], p [t "regular"]])
  , ("regular\n\n  blockquote\n  and more\n\nregular",                          d [ p [t "regular"], bq [ p [t "blockquote and more"]], p [t "regular"]])
  , ("regular\n\n  blockquote\n  and more\n\n     verbatim\n\nregular",         d [ p [t "regular"], bq [ p [t "blockquote and more"], v "verbatim" ], p [t "regular"]])
  , ("regular\n\n  blockquote\n  and more\n\n    nested blockquote\n\nregular", d [ p [t "regular"], bq [ p [t "blockquote and more"], bq [p [t "nested blockquote" ]]], p [t "regular"]])
  , ("regular\n\n  blockquote\n  and more\n\n  second paragraph\n\nregular",    twoParagraphBq)
  , ("regular\n\n  blockquote\n  and more\n \n  second paragraph\n\nregular",   twoParagraphBq)
  , ("regular\n\n  blockquote\n  and more\n  \n  second paragraph\n\nregular",  twoParagraphBq)
  , ("p\n\n  A bq 1\n\n    B bq 2\n\n      C bq 3\n\n    D bq 2\n\np", d [ p [t "p"], bq [ p [t "A bq 1"], bq [ p [t "B bq 2"], bq [ p [t "C bq 3"] ], p [t "D bq 2"]]], p [t "p"]])
  , ("p\n\n  A bq 1\n\n    B bq 2\n\n      C bq 3\n\n    D bq 2\n\np", d [ p [t "p"], bq [ p [t "A bq 1"], bq [ p [t "B bq 2"], bq [ p [t "C bq 3"] ], p [t "D bq 2"]]], p [t "p"]])
  , ("p\n\n  A bq 1\n\n    B bq 2\n\n      C bq 3\n\n    D bq 2\n\n  E bq 1\n\np", d [ p [t "p"], bq [ p [t "A bq 1"], bq [ p [t "B bq 2"], bq [ p [t "C bq 3"] ], p [t "D bq 2"]], p [t "E bq 1"]], p [t "p"]])
  , ("foo\n\n  A bq 1\n\n    B bq 2\n\n      C bq 3\n\n    D bq 2\n\np",
     d [ p [t "foo"], bq [ p [t "A bq 1"], bq [ p [t "B bq 2"], bq [ p [t "C bq 3"]], p [t "D bq 2"]]], p [t "p"]])
  , ("bar\n\n  A bq 1\n\n    B bq 2\n\n      C bq 3\n\n         Xv1\n\n    D bq 2\n\np",
     d [ p [t "bar"], bq [ p [t "A bq 1"], bq [ p [t "B bq 2"], bq [ p [t "C bq 3"], v "Xv1"], p [t "D bq 2"]]], p [t "p"]])
  , ("baz\n\n  A bq 1\n\n    B bq 2\n\n      C bq 3\n\n         Xv1\n          Yv2\n         Zv3\n\n    D bq 2\n\np",
     d [ p [t "baz"], bq [ p [t "A bq 1"], bq [ p [t "B bq 2"], bq [ p [t "C bq 3"], v "Xv1\n Yv2\nZv3" ], p [t "D bq 2"]]], p [t "p"]])
  , ("quux\n\n  A bq 1\n\n    B bq 2\n\n      C bq 3\n\n         Xv1\n          Yv2\n         Zv3\n\n    D bq 2\n\n  E bq 1\n\np",
     d [ p [t "quux"], bq [ p [t "A bq 1"], bq [ p [t "B bq 2"], bq [ p [t "C bq 3"], v "Xv1\n Yv2\nZv3" ], p [t "D bq 2"]], p [t "E bq 1"]], p [t "p"]])
  , ("  - foo\n\n  - bar\n\n", d [ ul [ li [ p [t "foo"]], li [p [t "bar"]]]])
  , ("  # foo\n\n  # bar\n\n", d [ ol [ li [ p [t "foo"]], li [p [t "bar"]]]])
  ]

threeLineVerbatim = d [ v "line one\n line two\n line three"]
twoParagraphBq = d [ p [t "regular"], bq [ p [t "blockquote and more"], p [t "second paragraph" ]], p [t "regular"]]
verbatimThenParagraph = d [ v "verbatim", p [ t "foo"]]

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

testParse = runParser document (0, 0, 0) "input string"

check (input, expected) =
  case testParse input of
    Left err -> (False, show err)
    Right d ->
      if d == expected then
        (True, "")
      else
        (False, "  Exp: " ++ show expected ++ "\n  Got: " ++ show d)
