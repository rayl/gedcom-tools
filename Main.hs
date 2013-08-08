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
import System.Environment (getArgs)
import Text.Parsec.Char (char,oneOf,string)
import Text.Parsec.Combinator (eof,many1,option,optional,optionMaybe)
import Text.Parsec.Pos (SourcePos,sourceLine)
import Text.Parsec.Prim (Parsec,runParser,(<|>),many,try,getPosition,token,getState,modifyState)
import Text.Parsec.String (Parser,GenParser,parseFromFile)
import Text.Printf (printf)


infile = "rayl2.ged"


-- | Parse infile into many GedLine objects
main :: IO ()
main = do
    args <- getArgs
    if length args > 0
        then run gedFile infile
        else run2 infile


-- | Parse file f using Parser p, printing a parse error or returning
-- the result of a successful parse
run :: Show a => Parser a -> FilePath -> IO ()
run p f = do
  x <- parseFromFile p f
  case x of
    Left e -> putStr "Parse error at " >> print e
    Right r -> print r

-- | Parse file f using Parser p, printing a parse error or returning
-- the result of a successful parse
run2 :: FilePath -> IO ()
run2 f = do
    ls <- parseFromFile gedLines f
    case ls of
        Left e -> putStr "Parse error at " >> print e
        Right r -> do
            putStrLn $ "Parsed " ++ (show . length) r ++ " lines"
            let rs = runParser sLineageLinkedGedcom 0 f r
            case rs of
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

data     GedLine    = GedLine SourcePos Level (Maybe XrefId) Tag (Maybe LineValue)
type     Level      = Int
newtype  XrefId     = XrefId String
newtype  Tag        = Tag String
data     LineValue  = LvPtr Pointer | LvLineItem String
newtype  Pointer    = Pointer String

instance Show GedLine where
    show (GedLine p l x t v) = "\n  " ++ ln ++ "> " ++ r t ++ spc ++ q x ++ q v
      where
        ln = printf "%5d" $ sourceLine p
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
    p <- getPosition
    l <- level
    x <- optional_xref_id
    t <- tag
    v <- optional_line_value
    terminator
    return $ GedLine p l x t v
    
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

level :: Parser Level
level = level0 <|> leveln

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


-----------------------------------------------------------------------------
-- Chapter 2, Lineage Linked Grammar

-- parse a GEDCOM file into a list of GedLine objects
gedLines :: Parser [GedLine]
gedLines = do
    x <- many $ gedcom_line level
    eof
    return x
    
type LLParser a = GenParser GedLine Level a
type Rec = LLParser Bool

-- record repeaters
r01,r03,r11,r0m,r1m,chk :: String -> Rec
r01 t = option False (chk t)
r03   = r01
r11   = chk
r0m   = r01 -- must repeat body for each parent
r1m   = r11
chk t = do
    n <- getState
    let pos (GedLine x _ _ _ _) = x
        test (GedLine _ l _ (Tag a) _) =
            if l == n && a == t
            then Just True
            else Nothing
    token show pos test


-- structure repeaters
s01,s11,s0m,s1m :: Rec -> Rec
s01 b = option False b
s11   = id
s0m b = many  b >> return True
s1m b = many1 b >> return True



-- execute child at next level when parent record exists
when :: Rec -> Rec -> Rec
when parent child = do
    x <- parent
    if x
      then do
        modifyState $ \x -> x + 1
        y <- child
        modifyState $ \x -> x - 1
        return y
      else return False



xxx = r11 "FIXME"


-- Record Structures, page 23

sLineageLinkedGedcom = do
    s11 sHeader
    s01 sSubmissionRecord
    s1m sRecord
    r11 "TRLR"

sHeader =
    when (r11 "HEAD") $ do
        when (r11 "SOUR") $ do
            r01 "VERS"
            r01 "NAME"
            when (r01 "CORP") $ do
                s01 sAddressStructure
            when (r01 "DATA") $ do
                r01 "DATE"
                r01 "COPR"
        r01 "DEST"
        when (r01 "DATE") $ do
            r01 "TIME"
        r11 "SUBM"
        r01 "SUBN"
        r01 "FILE"
        r01 "COPR"
        when (r11 "GEDC") $ do
            r11 "VERS"
            r11 "FORM"
        when (r11 "CHAR") $ do
            r01 "VERS"
        r01 "LANG"
        when (r01 "PLAC") $ do
            r11 "FORM"
        when (r01 "NOTE") $ do
            r0m ("CONT" ++ "CONC") -- fixme

sRecord = s11 sFamRecord <|>
          s11 sIndividualRecord <|>
          s1m sMultimediaRecord <|>
          s11 sNoteRecord <|>
          s11 sRepositoryRecord <|>
          s11 sSourceRecord <|>
          s11 sSubmitterRecord

sFamRecord = xxx

sIndividualRecord =
    when (r11 "INDI") $ do
        r01 "RESN"
        s0m sPersonalNameStructure
        r01 "SEX"
        s0m sIndividualEventStructure
        s0m sIndividualAttributeStructure
        s0m sLdsIndividualOrdinance
        s0m sChildToFamilyLink
        s0m sSpouseToFamilyLink
        r0m "SUBM"
        s0m sAssociationStructure
        r0m "ALIA"
        r0m "ANCI"
        r0m "DESI"
        s0m sSourceCitation
        s0m sMultimediaLink
        s0m sNoteStructure
        r01 "RFN"
        r01 "AFN"
        when (r0m "REFN") $ do
            r01 "TYPE"
        r01 "RIN"
        s01 sChangeDate

sMultimediaRecord = xxx

sNoteRecord = xxx

sRepositoryRecord = xxx

sSourceRecord = xxx

sSubmissionRecord =
    when (r11 "SUBN") $ do
        r01 "SUBM"
        r01 "FAMF"
        r01 "TEMP"
        r01 "ANCE"
        r01 "DESC"
        r01 "ORDI"
        r01 "RIN"

sSubmitterRecord =
    when (r11 "SUBM") $ do
        r11 "NAME"
        s01 sAddressStructure
        s0m sMultimediaLink
        r03 "LANG"
        r01 "RFN"
        r01 "RIN"
        s01 sChangeDate



-- substructures, page 29

sAddressStructure = do
    when (r01 "ADDR") $ do
        r0m "CONT"
        r01 "ADR1"
        r01 "ADR2"
        r01 "CITY"
        r01 "STAE"
        r01 "POST"
        r01 "CTRY"
    r03 "PHON"

sAssociationStructure = xxx

sChangeDate =
    when (r11 "CHAN") $ do
        when (r11 "DATE") $ do
            r01 "TIME"
        s0m sNoteStructure

sChildToFamilyLink =
    when (r11 "FAMC") $ do
        r0m "PEDI"
        s0m sNoteStructure

sEventDetail = xxx

sFamilyEventStructure = xxx

sIndividualAttributeStructure = xxx

sIndividualEventStructure = xxx

sLdsIndividualOrdinance = xxx

sLdsSpouseSealing = xxx

sMultimediaLink = xxx

sNoteStructure = xxx

sPersonalNameStructure =
    when (r11 "NAME") $ do
        r01 "NPFX"
        r01 "GIVN"
        r01 "NICK"
        r01 "SPFX"
        r01 "SURN"
        r01 "NSFX"
        s0m sSourceCitation
        s0m sNoteStructure

sPlaceStructure = xxx

sSourceCitation = xxx

sSourceRepositoryCitation = xxx

sSpouseToFamilyLink =
    when (r11 "FAMS") $ do
        s0m sNoteStructure
        


-- primitive elements, page 37
