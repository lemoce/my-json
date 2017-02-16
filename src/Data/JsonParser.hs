module Data.JsonParser (encode) where

import           Control.Applicative
import           Data.Char           (chr, isAlphaNum, isControl, isDigit,
                                      isPunctuation)
import           Data.Json
import qualified Data.List           as L
import           Data.Parser
import           Prelude             hiding (takeWhile)

nullParser :: Parser JSON
nullParser = do
  space
  string "null"
  space
  return jnull

boolParser :: Parser JSON
boolParser = do
  space
  b <- string "true" <|> string "false"
  space
  return $ jbool (b == "true")

numberParser :: Parser JSON
numberParser = do
  space
  r <- fromIntegral `fmap` int
  f <- (\s -> (read ("0." ++ s) :: Double)) `fmap` ((char '.' >> many digit) <|> pure "0")
  exp <- fromIntegral `fmap` (((char 'e' <|> char 'E') >> int) <|> pure 0)
  space
  return $ jnumber $ (if r > 0 then (r +) else (r -)) f * (10 ** exp)

escapedChar :: Parser Char
escapedChar = do
  escape <- char '\\'
  myChar <- item
  if myChar == 'u'
     then do unicode <- nat
             return $ chr unicode
     else do return myChar

stringParser :: Parser JSON
stringParser = do
  space
  char '"'
  str <- many (escapedChar <|>
               (sat (\ch -> isAlphaNum ch ||
                      (isPunctuation ch && ch `notElem` "/],:\"\\"))))
  char '"'
  space
  return $ jstring str

keyJValuePairParser :: Parser (String, JSON)
keyJValuePairParser = do
  space
  char '"'
  str <- filter ('"' /=) <$> takeWhile (':' /=)
  char ':'
  space
  jvalue <- jsonParser
  _ <- char ',' <|> pure ' '
  space
  return $ (str, jvalue)

objectParser :: Parser JSON
objectParser = do
  space
  char '{'
  list <- many keyJValuePairParser
  char '}'
  space
  return $ jobject list

arrayLineParser :: Parser JSON
arrayLineParser = do
  space
  jvalue <- jsonParser
  _ <- char ',' <|> pure ' '
  return jvalue

arrayParser :: Parser JSON
arrayParser = do
  space
  char '['
  list <- many arrayLineParser
  space
  char ']'
  space
  return $ jarray list

jsonParser :: Parser JSON
jsonParser = objectParser <|> nullParser
  <|> boolParser <|> numberParser
  <|> stringParser
  <|> arrayParser

encode :: String -> Maybe JSON
encode initial =
  let result = parse (jsonParser) initial
  in if L.null result
        then Nothing
        else Just $ (fst . head) result
