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



gedFile :: Parser [GedLine]
gedFile = do
    x <- many gedLine
    eof
    return x

gedLine :: Parser GedLine
gedLine = do
    l <- level
    x <- optionMaybe $ xrefid
    t <- tag
    v <- optionMaybe $ toEol
    eol
    return $ GedLine l x t v
    
data GedLine   = GedLine Level (Maybe XrefId) Tag (Maybe LineValue) deriving (Show)
type Level     = Int
type XrefId    = String
type Tag       = String
type LineValue = String

level :: Parser Level
level = do
    x <- many1 digit
    ws
    return $ read x

xrefid :: Parser XrefId
xrefid = do
    char '@' 
    x <- many1 alphaNum
    char '@'
    ws
    return x

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
