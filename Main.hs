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
import Text.Parsec.Char (char,oneOf,string)
import Text.Parsec.Combinator (eof,many1,option,optionMaybe)
import Text.Parsec.Prim (runParser,(<|>),(<?>),many,try,getState,modifyState,unexpected)
import Text.Parsec.String (GenParser)
import Text.Printf (printf)


-----------------------------------------------------------------------------
infile = "rayl2.ged"

main :: IO ()
main = do
    c <- readFile infile
    let rs = runParser gedFile 0 infile c
    case rs of
        Left e -> putStr "Parse error at " >> print e
        Right r -> print r



-----------------------------------------------------------------------------
type GedParser a = GenParser Char Int a
type GedChecker = GedParser Bool


gedFile :: GedChecker
gedFile = sLineageLinkedGedcom >> eof >> return True


-- repeaters
r01,r03,r11,r0m,r1m :: GedChecker -> GedChecker
r01 b = option False b
r03 b = r01 b >> r01 b >> r01 b
r11 b = b
r0m b = many  b >> return True
r1m b = many1 b >> return True


-- execute child at next level when parent record exists
when :: GedChecker -> GedChecker -> GedChecker
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
loop :: GedChecker -> GedChecker -> GedChecker
loop parent child = whileM_ (when parent child) (return True) >> return True


chk :: String -> GedChecker
chk t = try line <?> (t ++ " record")
  where
    line = do
        n <- getState

        l <- level
        if l == n
          then return ()
          else unexpected $ "level " ++ show l ++ ", should be " ++ show n

        optional_xref_id
        
        x <- tag
        if x == t
          then return ()
          else unexpected $ "tag " ++ x ++ ", should be " ++ t

        optional_line_value
        
        terminator
        
        return True


xxx = chk "FIXME"




-----------------------------------------------------------------------------
-- Chapter 1 - Data Representation Grammar
-----------------------------------------------------------------------------

-- Grammar Syntax, page 11

alpha :: GedParser Char
alpha = oneOf $ concat [ ['A'..'Z'] , ['a'..'z'] , ['_']]

alphanum :: GedParser Char
alphanum = alpha <|> digit

any_char :: GedParser Char
any_char = alpha <|> digit <|> otherchar <|> char '#' <|> char ' ' <|> (char '@' >> char '@')
           -- add Tab char, since Gramps puts out tabs in notes
           <|> char '\t'

delim :: GedParser Char
delim = char ' '

digit :: GedParser Char
digit = oneOf ['0'..'9']

-- escape
-- escape_text
-- level

level :: GedParser Int
level = (level0 <|> leveln) >>= return . read
  where
    level0 = string "0"
    leveln = do
        x <- oneOf ['1'..'9']
        y <- many digit
        return $ x:y

line_item :: GedParser String
line_item = many any_char -- no support for escape sequences

line_value :: GedParser String
line_value = pointer <|> line_item

non_at :: GedParser Char
non_at = alpha <|> digit <|> otherchar <|> char '#' <|> char ' ' 

-- null

optional_line_value :: GedParser (Maybe String)
optional_line_value = optionMaybe $ try $ delim >> line_value

optional_xref_id :: GedParser (Maybe String)
optional_xref_id = optionMaybe $ try $ delim >> xref_id

otherchar :: GedParser Char
otherchar = oneOf $ map chr $ concat
            [ [0x21..0x22] , [0x24..0x2f] , [0x3a..0x3f]
            , [0x5b..0x5e] , [0x60] , [0x7b..0x7e] , [0x80..0xfe]
            ]

pointer :: GedParser String
pointer = do
    char '@'
    x <- alphanum
    y <- many non_at
    char '@'
    return (x:y)

-- pointer_char
-- pointer_string

tag :: GedParser String
tag = delim >> many1 alphanum

terminator :: GedParser Char
terminator = cr <|> lf <|> (cr >> lf) <|> (lf >> cr)
  where
    cr = char '\r'
    lf = char '\n'

xref_id :: GedParser String
xref_id = pointer





-----------------------------------------------------------------------------
-- Chapter 2 - Lineage Linked Grammar
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Record Structures, page 23

sLineageLinkedGedcom = do
    r11 sHeader
    r01 sSubmissionRecord
    r1m sRecord
    r11 rTRLR

sHeader =
    when (r11 rHEAD) $ do
        when (r11 rSOUR) $ do
            r01 rVERS
            r01 rNAME
            when (r01 rCORP) $ do
                r01 sAddressStructure
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
    r11 sFamRecord <|>
    r11 sIndividualRecord <|>
    r1m sMultimediaRecord <|>
    r11 sNoteRecord <|>
    r11 sRepositoryRecord <|>
    r11 sSourceRecord <|>
    r11 sSubmitterRecord

sFamRecord =
    when (r11 rFAM) $ do
        loop (r01 sFamilyEventStructure) $ do
            when (r01 rHUSB) $ do
                r11 rAGE
            when (r01 rWIFE) $ do
                r11 rAGE
        r01 rHUSB
        r01 rWIFE
        r0m rCHIL
        r01 rNCHI
        r0m rSUBM
        r0m sLdsSpouseSealing
        r0m sSourceCitation
        r0m sMultimediaLink
        r0m sNoteStructure
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        r01 sChangeDate        

sIndividualRecord =
    when (r11 rINDI) $ do
        r01 rRESN
        r0m sPersonalNameStructure
        r01 rSEX
        r0m sIndividualEventStructure
        r0m sIndividualAttributeStructure
        r0m sLdsIndividualOrdinance
        r0m sChildToFamilyLink
        r0m sSpouseToFamilyLink
        r0m rSUBM
        r0m sAssociationStructure
        r0m rALIA
        r0m rANCI
        r0m rDESI
        r0m sSourceCitation
        r0m sMultimediaLink
        r0m sNoteStructure
        r01 rRFN
        r01 rAFN
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        r01 sChangeDate

sMultimediaRecord = xxx

sNoteRecord =
    when (r11 rNOTE) $ do
        r0m (rCONC <|> rCONT)
        r0m sSourceCitation
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        r01 sChangeDate

sRepositoryRecord =
    when (r11 rREPO) $ do
        r01 rNAME
        r01 xWWW
        r01 sAddressStructure
        r0m sNoteStructure
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        r01 sChangeDate

sSourceRecord =
    when (r11 rSOUR) $ do
        when (r01 rDATA) $ do
            loop (r01 rEVEN) $ do
                r01 rDATE
                r01 rPLAC
            r01 rAGNC
            r0m sNoteStructure
        when (r01 rAUTH) $ do
            r0m (rCONT <|> rCONC)
        when (r01 rTITL) $ do
            r0m (rCONT <|> rCONC)
        r01 rABBR
        when (r01 rPUBL) $ do
            r0m (rCONT <|> rCONC)
        when (r01 rTEXT) $ do
            r0m (rCONT <|> rCONC)
        r01 sSourceRepositoryCitation
        r0m sMultimediaLink
        r0m sNoteStructure
        loop (r01 rREFN) $ do
            r01 rTYPE
        r01 rRIN
        r01 sChangeDate

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
        r01 sAddressStructure
        r0m sMultimediaLink
        r03 rLANG
        r01 rRFN
        r01 rRIN
        r01 sChangeDate



-----------------------------------------------------------------------------
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
        r0m sNoteStructure

sChildToFamilyLink =
    when (r11 rFAMC) $ do
        r0m rPEDI
        r0m sNoteStructure

sEventDetail = do
    r01 rTYPE
    r01 rDATE
    r01 sPlaceStructure
    r01 sAddressStructure
    r01 rAGE
    r01 rAGNC
    r01 rCAUS
    r0m sSourceCitation
    r0m sMultimediaLink
    r0m sNoteStructure

sFamilyEventStructure =
    when (r11 rList) $ do
        r01 sEventDetail
          where
            rList = rANUL <|> rCENS <|> rDIV  <|> rDIVF <|>
                    rENGA <|> rMARR <|> rMARB <|> rMARC <|>
                    rMARL <|> rMARS <|>
                    rEVEN

sIndividualAttributeStructure =
    when (r11 rList) $ do
        r01 sEventDetail
          where
            rList = rCAST <|> rDSCR <|> rEDUC <|> rIDNO <|>
                    rNATI <|> rNCHI <|> rNMR  <|> rOCCU <|>
                    rPROP <|> rRELI <|> rRESI <|> rSSN  <|>
                    rTITL
            
sIndividualEventStructure =
    c1 <|> c2 <|> c3
      where
        c1 = when (r11 $ rBIRT <|> rCHR) $ do
            r01 sEventDetail
            r01 rFAMC

        c2 = when (r11 $ rADOP) $ do
            r01 sEventDetail
            when (r01 rFAMC) $ do
                r01 rADOP
    
        c3 = when (r11 rList) $ do
            r01 sEventDetail
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
            r0m sNoteStructure

        c2 = r11 rOBJE

sNoteStructure =
    try c1 <|> try c2
      where
        c1 = when (r11 rNOTE) $ do
            r0m sSourceCitation

        c2 = when (r11 rNOTE) $ do
            r0m (rCONC <|> rCONT)
            r0m sSourceCitation

sPersonalNameStructure =
    when (r11 rNAME) $ do
        r01 rNPFX
        r01 rGIVN
        r01 rNICK
        r01 rSPFX
        r01 rSURN
        r01 rNSFX
        r0m sSourceCitation
        r0m sNoteStructure

sPlaceStructure =
    when (r11 rPLAC) $ do
        when (r01 xMAP) $ do
            r01 xLATI
            r01 xLONG
        r01 rFORM
        r0m sSourceCitation
        r0m sNoteStructure

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
            r0m sMultimediaLink
            r0m sNoteStructure

        c2 = when (r11 rSOUR) $ do
            r0m (rCONC <|> rCONT)
            loop (r01 rTEXT) $ do
                r0m (rCONC <|> rCONT)
            r0m sNoteStructure

            
sSourceRepositoryCitation =
    when (r11 rREPO) $ do
        r0m sNoteStructure
        loop (r01 rCALN) $ do
            r01 rMEDI

sSpouseToFamilyLink =
    when (r11 rFAMS) $ do
        r0m sNoteStructure
        


-----------------------------------------------------------------------------
-- primitive elements, page 37





-----------------------------------------------------------------------------
-- Appendix A - Tag Definition
-----------------------------------------------------------------------------

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
