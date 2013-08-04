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

import Data.Char (chr)
import Control.Monad
import Text.Parsec.Token
import Text.ParserCombinators.Parsec hiding (digit)


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



-----------------------------------------------------------------------------
-- File and Record data structure
--   Just a quick hack for pretty printing at the moment.

data GedFile   = GedFile [GedRecord]
data GedRecord = GedRecord [GedLine]

instance Show GedFile where
    show (GedFile f) = "\n" ++ (concat (map show f))

instance Show GedRecord where
    show (GedRecord l) = "\n" ++ (take 80 (repeat '-')) ++ (concat (map show l)) ++ "\n"

gedFile :: Parser GedFile
gedFile = do
    x <- many gedRecord
    eof
    return $ GedFile x

gedRecord :: Parser GedRecord
gedRecord = do
    x <- gedcom_line level0
    y <- many $ gedcom_line leveln
    return $ GedRecord (x:y)



-----------------------------------------------------------------------------
-- Data structure to hold a single line read from a GEDCOM 5.5 file

data     GedLine    = GedLine Level (Maybe XrefId) Tag (Maybe LineValue)
type     Level      = Int
newtype  XrefId     = XrefId String
newtype  Tag        = Tag String
data     LineValue  = LvPtr Pointer | LvLineItem String
newtype  Pointer    = Pointer String

instance Show GedLine where
    show (GedLine l x t v) = "\n  " ++ r t ++ spc ++ q x ++ q v
      where
        spc = take (2*(l+1)) $ repeat ' '
        r x = show x ++ " "
        q x = maybe "" r x

instance Show XrefId where
    show (XrefId x) = "[" ++ x ++ "]"

instance Show Tag where
    show (Tag x) = x ++ ":"

instance Show LineValue where
    show (LvPtr p) = show p
    show (LvLineItem l) = l

instance Show Pointer where
    show (Pointer x) = "<" ++ x ++ ">"


-- Parsers derived from GEDCOM 5.5 grammar syntax, except escape
-- sequences. also, delim handlnig is slightly rearranged in
-- gedcom_line and immediate subproductions

gedcom_line :: Parser Level -> Parser GedLine
gedcom_line level = do
    l <- level
    x <- optional_xref_id
    t <- tag
    v <- optional_line_value
    terminator
    return $ GedLine l x t v
    
alpha :: Parser Char
alpha = oneOf $ concat [ ['A'..'Z'] , ['a'..'z'] , ['_']]

alphanum :: Parser Char
alphanum = alpha <|> digit

any_char :: Parser Char
any_char = alpha <|> digit <|> otherchar <|> char '#' <|> char ' ' <|> (char '@' >> char '@')
           -- add Tab char, since Gramps puts out tabs in notes
           <|> char '\t'

delim :: Parser Char
delim = char ' '

digit :: Parser Char
digit = oneOf ['0'..'9']

-- escape
-- escape_text
-- level

level0 :: Parser Level
level0 = string "0" >> return 0

leveln :: Parser Level
leveln = do
    x <- oneOf ['1'..'9']
    y <- many digit
    return $ read (x:y)

line_item :: Parser String
line_item = many any_char -- no support for escape sequences

line_value :: Parser LineValue
line_value = lvp <|> lvi
  where lvp = pointer   >>= return . LvPtr
        lvi = line_item >>= return . LvLineItem

non_at :: Parser Char
non_at = alpha <|> digit <|> otherchar <|> char '#' <|> char ' ' 

-- null

optional_line_value :: Parser (Maybe LineValue)
optional_line_value = optionMaybe $ try $ delim >> line_value

optional_xref_id :: Parser (Maybe XrefId)
optional_xref_id = optionMaybe $ try $ delim >> xref_id

otherchar :: Parser Char
otherchar = oneOf $ map chr $ concat
            [ [0x21..0x22] , [0x24..0x2f] , [0x3a..0x3f]
            , [0x5b..0x5e] , [0x60] , [0x7b..0x7e] , [0x80..0xfe]
            ]

pointer :: Parser Pointer
pointer = _pointer >>= return . Pointer

-- reuse this production for xref_id
_pointer :: Parser String
_pointer = do
    char '@'
    x <- alphanum
    y <- many non_at
    char '@'
    return (x:y)

-- pointer_char
-- pointer_string

tag :: Parser Tag
tag = delim >> many1 alphanum >>= return . Tag

terminator :: Parser ()
terminator = (cr <|> lf <|> (cr >> lf) <|> (lf >> cr)) >> return ()
  where
    cr = char '\r'
    lf = char '\n'

xref_id :: Parser XrefId
xref_id = _pointer >>= return . XrefId
