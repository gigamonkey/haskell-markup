import Control.Monad
import Markup
import System.Environment
import Text.Parsec

data Result = Okay Markup | BadParse ParseError String

-- Parse test files ----------------------------------------------------

checkParse a input =
  case markup a input of
    Right m -> Okay m
    Left e  -> BadParse e input

checkFile a = do
  markup <- readFile a
  case checkParse a markup of
    Okay m -> putStrLn $ a ++ " okay:\n\n" ++ show m
    BadParse e s -> putStrLn $ "Parser error: " ++ show e ++ "\n\n--------\n" ++ s ++ "\n--------"

main = do
  args <- getArgs
  mapM_ checkFile args
