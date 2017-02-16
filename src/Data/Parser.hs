module Data.Parser where

import           Control.Applicative
import           Data.Char

import qualified Data.List           as L

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap g p = P (\inp -> case parse p inp of
                          []         -> []
                          [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])

  pg <*> px = P (\inp -> case parse pg inp of
                           []         -> []
                           [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  p >>= f = P (\inp -> case parse p inp of
                         []         -> []
                         [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  empty = P (\inp -> [])

  p <|> q = P (\inp -> case parse p inp of
                         []         -> parse q inp
                         [(v, out)] -> [(v, out)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x, xs)])

peek :: Char -> Parser ()
peek ch = P (\inp -> [((), (ch:inp))])

takeWhile :: (Char -> Bool) -> Parser String
takeWhile fun = P (\inp -> case inp of
                             [] -> []
                             xs -> [(L.takeWhile fun xs, L.dropWhile (\ch -> fun ch) xs)])

three :: Parser (Char, Char)
three = do
  x <- item
  _ <- item
  z <- item
  return (x, z)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x
               then return x
               else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <-  char x
                   _ <-  string xs
                   return (x:xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

int :: Parser Int
int = do _ <-  char '-'
         n <- nat
         return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier = token ident

natural = token nat

integer = token int

symbol xs = token (string xs)

nats = do _ <- symbol "["
          n <- natural
          ns <- many (do _ <- symbol ","
                         natural)
          _ <- symbol "]"
          return (n:ns)
