import Text.ParserCombinators.Parsec

document :: GenParser Char st String
document = do
  text <- text
  eof
  return text

text :: GenParser Char st String
text = many anyChar

main = do
  putStrLn $ case testParse "hello, world!" of
    Left err  -> "Error: " ++ show err
    Right doc -> "Document: " ++ show doc

testParse :: String -> Either ParseError String
testParse input = parse document "(unknown)" input
