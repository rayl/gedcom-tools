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
            let rs = runParser llGedcom 0 f r
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


req :: String -> Rec
req = chk

opt :: String -> Rec
opt t = option False $ chk t

optm = opt

chk :: String -> Rec
chk t = do
    n <- getState
    let pos (GedLine x _ _ _ _) = x
        test (GedLine _ l _ (Tag a) _) =
            if l == n && a == t
            then Just True
            else Nothing
    token show pos test


when :: Rec -> Rec -> Rec
when t b = do
    x <- t
    if x
      then do
        modifyState $ \x -> x + 1
        y <- b
        modifyState $ \x -> x - 1
        return y
      else return False

req1 :: Rec -> Rec
req1 = id

reqM :: Rec -> Rec
reqM b = many1 b >> return True

opt1 :: Rec -> Rec
opt1 b = option False b

optM :: Rec -> Rec
optM b = many b >> return True

llGedcom :: Rec
llGedcom = do
    req1 llHeader
    opt1 llSubmissionRecord
    reqM llRecord
    req "TRLR"

llHeader :: Rec
llHeader =
    when (req "HEAD") $ do
        when (req "SOUR") $ do
            opt "VERS"
            opt "NAME"
            when (opt "CORP") $ do
                opt1 llAddressStructure
            when (opt "DATA") $ do
                opt "DATE"
                opt "COPR"
        opt "DEST"
        when (opt "DATE") $ do
            opt "TIME"
        req "SUBM"
        opt "SUBN"
        opt "FILE"
        opt "COPR"
        when (req "GEDC") $ do
            req "VERS"
            req "FORM"
        when (req "CHAR") $ do
            opt "VERS"
        opt "LANG"
        when (opt "PLAC") $ do
            req "FORM"
        when (opt "NOTE") $ do
            opt "CONT" -- FIXME        

llRecord :: Rec
llRecord = --req1 llFamRecord <|>
           req1 llIndividualRecord <|>
           --reqM llMultimediaRecord <|>
           --req1 llNoteRecord <|>
           --req1 llRepositoryRecord <|>
           --req1 llSourceRecord <|>
           req1 llSubmitterRecord

--llFamRecord = return True

llIndividualRecord :: Rec
llIndividualRecord =
    when (req "INDI") $ do
        opt "RESN"
        optM llPersonalNameStructure
        opt "SEX"
        optM llIndividualEventStructure
        optM llIndividualAttributeStructure
        optM llLdsIndividualOrdinance
        optM llChildToFamilyLink
        optM llSpouseToFamilyLink
        optm "SUBM"
        optM llAssociationStructure
        optm "ALIA"
        optm "ANCI"
        optm "DESI"
        optM llSourceCitation
        optM llMultimediaLink
        optM llNoteStructure
        opt "RFN"
        opt "AFN"
        when (opt "REFN") $ do -- fixme
            opt "TYPE"
        opt "RIN"
        opt1 llChangeDate

--llMultimediaRecord = return True
--llNoteRecord = return True
--llRepositoryRecord = return True
--llSourceRecord = return True

llSubmissionRecord :: Rec
llSubmissionRecord =
    when (req "SUBN") $ do  -- fixme, xref
        opt "SUBM"
        opt "FAMF"
        opt "TEMP"
        opt "ANCE"
        opt "DESC"
        opt "ORDI"
        opt "RIN"

llSubmitterRecord :: Rec
llSubmitterRecord =
    when (req "SUBM") $ do -- fixme xref
        req "NAME"
        opt1 llAddressStructure
        optM llMultimediaLink
        opt "LANG"
        opt "RFN"
        opt "RIN"
        opt1 llChangeDate






xxx = req "FIXME"

llAddressStructure :: Rec
llAddressStructure = do
    when (req "ADDR") $ do -- fixme
        opt "CONT"
        opt "ADR1"
        opt "ADR2"
        opt "CITY"
        opt "STAE"
        opt "POST"
        opt "CTRY"
    opt "PHON"

llAssociationStructure = xxx

llChangeDate :: Rec
llChangeDate =
    when (req "CHAN") $ do
        when (req "DATE") $ do
            opt "TIME"
        optM llNoteStructure

llChildToFamilyLink :: Rec
llChildToFamilyLink =
    when (req "FAMC") $ do
        optm "PEDI"
        optM llNoteStructure

llIndividualAttributeStructure = xxx

llIndividualEventStructure :: Rec
llIndividualEventStructure = xxx

llLdsIndividualOrdinance = xxx

llMultimediaLink = xxx

llNoteStructure = xxx

llPersonalNameStructure :: Rec
llPersonalNameStructure =
    when (req "NAME") $ do
        opt "NPFX"
        opt "GIVN"
        opt "NICK"
        opt "SPFX"
        opt "SURN"
        opt "NSFX"
        optM llSourceCitation
        optM llNoteStructure
        
llSourceCitation = xxx

llSpouseToFamilyLink :: Rec
llSpouseToFamilyLink =
    when (req "FAMS") $ do
        optM llNoteStructure
        
