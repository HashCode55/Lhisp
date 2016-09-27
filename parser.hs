module Main where 
--get the env files 	
import System.Environment
import Monad	
--import the parsec library except the spaces function
import Text.ParserCombinators.Parsec hiding ( spaces )

--IO monad 
main :: IO()
main = do 
    args <- getArgs
    putStrLn (readExpr (args !! 0))

symbol :: Parser Char 
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

--parse is a function, symbol is a 
--it'll take a string and evaluate to a string 
readExpr :: String -> String 
--input is the string 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

--this is an action
spaces :: Parser()
spaces = skipMany1 space

--defining a data type 
data LispVal = Atom String 
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String 
    | Bool Bool

parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many (noneOf "\"")
    char '"'
    -- the symbol ahead is to avoid the parenthesis 
    return $ String x 

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many(letter <|> digit <|> symbol)    
    let atom = [first] ++ rest
    return $ case atom of 
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom atom    

--parse a number
parseNumber :: Parser LispVal
parseNumber = liftM(Number . read) $ many1 digit       

--combine all the parsers 
parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber 	



