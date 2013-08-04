-----------------------------------------------------------------------------
-- |
-- Module       : Main.hs
-- Description  : GEDCOM 5.5 processing tools
-- Copyright    : (c) 2013 Ray Lehtiniemi
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Ray Lehtiniemi <rayl@mail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-----------------------------------------------------------------------------

module Main (main) where

import Control.Monad
import Text.Parsec.Token
import Text.ParserCombinators.Parsec


infile = "rayl2.ged"


-- | Parse infile into many GedLine objects
main :: IO ()
main = run gedFile infile


-- | Parse file f using Parser p, printing a parse error or returning
-- the result of a successful parse
run :: Show a => Parser a -> FilePath -> IO ()
run p f = do
  x <- parseFromFile p f
  case x of
    Left e -> putStr "Parse error at " >> print e
    Right r -> print r



gedFile :: Parser [GedRecord]
gedFile = do
    x <- many gedRecord
    eof
    return x

gedRecord :: Parser GedRecord
gedRecord = do
    x <- gedLine level0
    y <- many $ gedLine leveln
    return $ GedRecord (x:y)

data GedRecord = GedRecord [GedLine]

instance Show GedRecord where
    show (GedRecord l) = "\nR:" ++ (concat (map show l))

gedLine :: Parser Level -> Parser GedLine
gedLine a = do
    l <- a
    x <- optionMaybe $ xrefid
    t <- tag
    v <- optionMaybe $ toEol
    eol
    return $ GedLine l x t v
    
data GedLine   = GedLine Level (Maybe XrefId) Tag (Maybe Value)
type Level     = Int
newtype XrefId = XrefId String
type Tag       = String
type Value     = String

instance Show GedLine where
    show (GedLine l x t v) = "\n" ++ spc ++ "L: " ++ q x ++ p t ++ q v
                             where
                               spc = take (2*(l+1)) $ repeat ' ' 
                               p x = show x ++ " "
                               q x = maybe "" p x

instance Show XrefId where
    show (XrefId x) = "[" ++ x ++ "]"

level0 :: Parser Level
level0 = do
    x <- string "0"
    ws
    return $ read x

leveln :: Parser Level
leveln = do
    x <- oneOf "123456789"
    y <- many digit
    ws
    return $ read $ x:y

xrefid :: Parser XrefId
xrefid = do
    char '@' 
    x <- many1 alphaNum
    char '@'
    ws
    return $ XrefId x

tag :: Parser Tag
tag = do
    x <- many1 upper
    ws
    return x

eol :: Parser String
eol = many1 $ oneOf "\r\n"

toEol :: Parser String
toEol = many1 $ noneOf "\r\n"

ws :: Parser String
ws = many $ char ' '
