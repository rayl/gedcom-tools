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

import Control.Monad.Loops (whileM_)
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
r01,r03,r11,r0m,r1m :: Rec -> Rec
r01 b = option False b
r03 b = r01 b >> r01 b >> r01 b
r11 b = b
r0m b = many  b >> return True
r1m b = many1 b >> return True


-- structure repeaters
s01,s11,s0m,s1m :: Rec -> Rec
s01 = r01
s11 = r11
s0m = r0m
s1m = r1m



-- execute child at next level when parent record exists
when :: Rec -> Rec -> Rec
when parent child = do
    x <- parent
    if x
      then do
        modifyState $ \x -> x + 1
        y <- child
        modifyState $ \x -> x - 1
        return True
      else return False

-- execute child at next level while parent records exist
loop :: Rec -> Rec -> Rec
loop parent child = whileM_ (when parent child) (return True) >> return True

    
chk :: String -> Rec
chk t = do
    n <- getState
    let pos (GedLine x _ _ _ _) = x
        test (GedLine _ l _ (Tag a) _) =
            if l == n && a == t
            then Just True
            else Nothing
    token show pos test



xxx = chk "FIXME"


-- Record Structures, page 23

sLineageLinkedGedcom = do
    s11 sHeader
    s01 sSubmissionRecord
    s1m sRecord
    r11 rTRLR

sHeader =
    when (r11 rHEAD) $ do
        when (r11 rSOUR) $ do
            r01 rVERS
            r01 rNAME
            when (r01 rCORP) $ do
                s01 sAddressStructure
            when (r01 rDATA) $ do
                r01 rDATE
                r01 rCOPR
        r01 rDEST
        when (r01 rDATE) $ do
            r01 rTIME
        r11 rSUBM
        r01 rSUBN
        r01 rFILE
        r01 rCOPR
        when (r11 rGEDC) $ do
            r11 rVERS
            r11 rFORM
        when (r11 rCHAR) $ do
            r01 rVERS
        r01 rLANG
        when (r01 rPLAC) $ do
            r11 rFORM
        when (r01 rNOTE) $ do
            r0m (rCONT <|> rCONC)

sRecord =
    s11 sFamRecord <|>
    s11 sIndividualRecord <|>
    s1m sMultimediaRecord <|>
    s11 sNoteRecord <|>
    s11 sRepositoryRecord <|>
    s11 sSourceRecord <|>
    s11 sSubmitterRecord

sFamRecord =
    when (r11 rFAM) $ do
        loop (s01 sFamilyEventStructure) $ do
            when (r01 rHUSB) $ do
                r11 rAGE
            when (r01 rWIFE) $ do
                r11 rAGE
        r01 rHUSB
        r01 rWIFE
        r0m rCHIL
        r01 rNCHI
        r0m rSUBM
        s0m sLdsSpouseSealing
        s0m sSourceCitation
        s0m sMultimediaLink
        s0m sNoteStructure
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        s01 sChangeDate        

sIndividualRecord =
    when (r11 rINDI) $ do
        r01 rRESN
        s0m sPersonalNameStructure
        r01 rSEX
        s0m sIndividualEventStructure
        s0m sIndividualAttributeStructure
        s0m sLdsIndividualOrdinance
        s0m sChildToFamilyLink
        s0m sSpouseToFamilyLink
        r0m rSUBM
        s0m sAssociationStructure
        r0m rALIA
        r0m rANCI
        r0m rDESI
        s0m sSourceCitation
        s0m sMultimediaLink
        s0m sNoteStructure
        r01 rRFN
        r01 rAFN
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        s01 sChangeDate

sMultimediaRecord = xxx

sNoteRecord =
    when (r11 rNOTE) $ do
        r0m (rCONC <|> rCONT)
        s0m sSourceCitation
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        s01 sChangeDate

sRepositoryRecord =
    when (r11 rREPO) $ do
        r01 rNAME
        r01 xWWW
        s01 sAddressStructure
        s0m sNoteStructure
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        s01 sChangeDate

sSourceRecord =
    when (r11 rSOUR) $ do
        when (r01 rDATA) $ do
            loop (r01 rEVEN) $ do
                r01 rDATE
                r01 rPLAC
            r01 rAGNC
            s0m sNoteStructure
        when (r01 rAUTH) $ do
            r0m (rCONT <|> rCONC)
        when (r01 rTITL) $ do
            r0m (rCONT <|> rCONC)
        r01 rABBR
        when (r01 rPUBL) $ do
            r0m (rCONT <|> rCONC)
        when (r01 rTEXT) $ do
            r0m (rCONT <|> rCONC)
        s01 sSourceRepositoryCitation
        s0m sMultimediaLink
        s0m sNoteStructure
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        s01 sChangeDate

sSubmissionRecord =
    when (r11 rSUBN) $ do
        r01 rSUBM
        r01 rFAMF
        r01 rTEMP
        r01 rANCE
        r01 rDESC
        r01 rORDI
        r01 rRIN
        
sSubmitterRecord =
    when (r11 rSUBM) $ do
        r11 rNAME
        s01 sAddressStructure
        s0m sMultimediaLink
        r03 rLANG
        r01 rRFN
        r01 rRIN
        s01 sChangeDate



-- substructures, page 29

sAddressStructure = do
    when (r01 rADDR) $ do
        r0m rCONT
        r01 rADR1
        r01 rADR2
        r01 rCITY
        r01 rSTAE
        r01 rPOST
        r01 rCTRY
    r03 rPHON

sAssociationStructure = xxx

sChangeDate =
    when (r11 rCHAN) $ do
        when (r11 rDATE) $ do
            r01 rTIME
        s0m sNoteStructure

sChildToFamilyLink =
    when (r11 rFAMC) $ do
        r0m rPEDI
        s0m sNoteStructure

sEventDetail = do
    r01 rTYPE
    r01 rDATE
    s01 sPlaceStructure
    s01 sAddressStructure
    r01 rAGE
    r01 rAGNC
    r01 rCAUS
    s0m sSourceCitation
    s0m sMultimediaLink
    s0m sNoteStructure

sFamilyEventStructure =
    when (r11 rList) $ do
        s01 sEventDetail
          where
            rList = rANUL <|> rCENS <|> rDIV  <|> rDIVF <|>
                    rENGA <|> rMARR <|> rMARB <|> rMARC <|>
                    rMARL <|> rMARS <|>
                    rEVEN

sIndividualAttributeStructure =
    when (r11 rList) $ do
        s01 sEventDetail
          where
            rList = rCAST <|> rDSCR <|> rEDUC <|> rIDNO <|>
                    rNATI <|> rNCHI <|> rNMR  <|> rOCCU <|>
                    rPROP <|> rRELI <|> rRESI <|> rSSN  <|>
                    rTITL
            
sIndividualEventStructure =
    c1 <|> c2 <|> c3
      where
        c1 = when (r11 $ rBIRT <|> rCHR) $ do
            s01 sEventDetail
            r01 rFAMC

        c2 = when (r11 $ rADOP) $ do
            s01 sEventDetail
            when (r01 rFAMC) $ do
                r01 rADOP
    
        c3 = when (r11 rList) $ do
            s01 sEventDetail
              where
                rList = rDEAT <|> rBURI <|> rCREM <|>
                        rBAPM <|> rBARM <|> rBASM <|> rBLES <|>
                        rCHRA <|> rCONF <|> rFCOM <|> rORDN <|>
                        rNATU <|> rEMIG <|> rIMMI <|>
                        rCENS <|> rPROB <|> rWILL <|>
                        rGRAD <|> rRETI <|>
                        rEVEN

sLdsIndividualOrdinance = xxx

sLdsSpouseSealing = xxx

sMultimediaLink =
    try c1 <|> try c2
      where
        c1 = when (r11 rOBJE) $ do
            r11 rFORM
            r01 rTITL
            r11 rFILE
            s0m sNoteStructure

        c2 = r11 rOBJE

sNoteStructure =
    try c1 <|> try c2
      where
        c1 = when (r11 rNOTE) $ do
            s0m sSourceCitation

        c2 = when (r11 rNOTE) $ do
            r0m (rCONC <|> rCONT)
            s0m sSourceCitation

sPersonalNameStructure =
    when (r11 rNAME) $ do
        r01 rNPFX
        r01 rGIVN
        r01 rNICK
        r01 rSPFX
        r01 rSURN
        r01 rNSFX
        s0m sSourceCitation
        s0m sNoteStructure

sPlaceStructure =
    when (r11 rPLAC) $ do
        when (r01 xMAP) $ do
            r01 xLATI
            r01 xLONG
        r01 rFORM
        s0m sSourceCitation
        s0m sNoteStructure

sSourceCitation =
    try c1 <|> try c2
      where
        c1 = when (r11 rSOUR) $ do
            r01 rPAGE
            when (r01 rEVEN) $ do
                r01 rROLE
            when (r01 rDATA) $ do
                r01 rDATE
                loop (r01 rTEXT) $ do
                    r0m (rCONC <|> rCONT)
            r01 rQUAY
            s0m sMultimediaLink
            s0m sNoteStructure

        c2 = when (r11 rSOUR) $ do
            r0m (rCONC <|> rCONT)
            loop (r01 rTEXT) $ do
                r0m (rCONC <|> rCONT)
            s0m sNoteStructure

            
sSourceRepositoryCitation =
    when (r11 rREPO) $ do
        s0m sNoteStructure
        loop (r01 rCALN) $ do
            r01 rMEDI

sSpouseToFamilyLink =
    when (r11 rFAMS) $ do
        s0m sNoteStructure
        


-- primitive elements, page 37





-- record types, page 69

rABBR = chk "ABBR"
rADDR = chk "ADDR"
rADR1 = chk "ADR1"
rADR2 = chk "ADR2"
rADOP = chk "ADOP"
rAFN  = chk "AFN"
rAGE  = chk "AGE"
rAGNC = chk "AGNC"
rALIA = chk "ALIA"
rANCE = chk "ANCE"
rANCI = chk "ANCI"
rANUL = chk "ANUL"
rASSO = chk "ASSO"
rAUTH = chk "AUTH"
rBAPL = chk "BAPL"
rBAPM = chk "BAPM"
rBARM = chk "BARM"
rBASM = chk "BASM"
rBIRT = chk "BIRT"
rBLES = chk "BLES"
rBLOB = chk "BLOB"
rBURI = chk "BURI"
rCALN = chk "CALN"
rCAST = chk "CAST"
rCAUS = chk "CAUS"
rCENS = chk "CENS"
rCHAN = chk "CHAN"
rCHAR = chk "CHAR"
rCHIL = chk "CHIL"
rCHR  = chk "CHR"
rCHRA = chk "CHRA"
rCITY = chk "CITY"
rCONC = chk "CONC"
rCONF = chk "CONF"
rCONL = chk "CONL"
rCONT = chk "CONT"
rCOPR = chk "COPR"
rCORP = chk "CORP"
rCREM = chk "CREM"
rCTRY = chk "CTRY"
rDATA = chk "DATA"
rDATE = chk "DATE"
rDEAT = chk "DEAT"
rDESC = chk "DESC"
rDESI = chk "DESI"
rDEST = chk "DEST"
rDIV  = chk "DIV"
rDIVF = chk "DIVF"
rDSCR = chk "DSCR"
rEDUC = chk "EDUC"
rEMIG = chk "EMIG"
rENDL = chk "ENDL"
rENGA = chk "ENGA"
rEVEN = chk "EVEN"
rFAM  = chk "FAM"
rFAMC = chk "FAMC"
rFAMF = chk "FAMF"
rFAMS = chk "FAMS"
rFCOM = chk "FCOM"
rFILE = chk "FILE"
rFORM = chk "FORM"
rGEDC = chk "GEDC"
rGIVN = chk "GIVN"
rGRAD = chk "GRAD"
rHEAD = chk "HEAD"
rHUSB = chk "HUSB"
rIDNO = chk "IDNO"
rIMMI = chk "IMMI"
rINDI = chk "INDI"
rLANG = chk "LANG"
rLEGA = chk "LEGA"
rMARB = chk "MARB"
rMARC = chk "MARC"
rMARL = chk "MARL"
rMARR = chk "MARR"
rMARS = chk "MARS"
rMEDI = chk "MEDI"
rNAME = chk "NAME"
rNATI = chk "NATI"
rNATU = chk "NATU"
rNCHI = chk "NCHI"
rNICK = chk "NICK"
rNMR  = chk "NMR"
rNOTE = chk "NOTE"
rNPFX = chk "NPFX"
rNSFX = chk "NSFX"
rOBJE = chk "OBJE"
rOCCU = chk "OCCU"
rORDI = chk "ORDI"
rORDN = chk "ORDN"
rPAGE = chk "PAGE"
rPEDI = chk "PEDI"
rPHON = chk "PHON"
rPLAC = chk "PLAC"
rPOST = chk "POST"
rPROB = chk "PROB"
rPROP = chk "PROP"
rPUBL = chk "PUBL"
rQUAY = chk "QUAY"
rREFN = chk "REFN"
rRELA = chk "RELA"
rRELI = chk "RELI"
rREPO = chk "REPO"
rRESI = chk "RESI"
rRESN = chk "RESN"
rRETI = chk "RETI"
rRFN  = chk "RFN"
rRIN  = chk "RIN"
rROLE = chk "ROLE"
rSEX  = chk "SEX"
rSLGC = chk "SLGC"
rSLGS = chk "SLGS"
rSOUR = chk "SOUR"
rSPFX = chk "SPFX"
rSSN  = chk "SSN"
rSTAE = chk "STAE"
rSTAT = chk "STAT"
rSUBM = chk "SUBM"
rSUBN = chk "SUBN"
rSURN = chk "SURN"
rTEMP = chk "TEMP"
rTEXT = chk "TEXT"
rTIME = chk "TIME"
rTITL = chk "TITL"
rTRLR = chk "TRLR"
rTYPE = chk "TYPE"
rVERS = chk "VERS"
rWIFE = chk "WIFE"
rWILL = chk "WILL"

-- gramps extensions
xMAP  = chk "MAP"
xLATI = chk "LATI"
xLONG = chk "LONG"
xWWW  = chk "WWW"
