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

import Control.Monad.Loops (iterateUntil)
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
n01,n03,n11,n0m,n1m :: GedChecker -> GedChecker
n01 b = option False b
n03 b = n01 b >> n01 b >> n01 b
n11 b = b
n0m b = many  b >> return True
n1m b = many1 b >> return True


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
each :: GedChecker -> GedChecker -> GedChecker
each parent child = iterateUntil not (when parent child)


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
    n11 sHeader
    n01 sSubmissionRecord
    n1m sRecord
    n11 rTRLR

sHeader =
    when (n11 rHEAD) $ do
        when (n11 rSOUR) $ do
            n01 rVERS
            n01 rNAME
            when (n01 rCORP) $ do
                n01 sAddressStructure
            when (n01 rDATA) $ do
                n01 rDATE
                n01 rCOPR
        n01 rDEST
        when (n01 rDATE) $ do
            n01 rTIME
        n11 rSUBM
        n01 rSUBN
        n01 rFILE
        n01 rCOPR
        when (n11 rGEDC) $ do
            n11 rVERS
            n11 rFORM
        when (n11 rCHAR) $ do
            n01 rVERS
        n01 rLANG
        when (n01 rPLAC) $ do
            n11 rFORM
        when (n01 rNOTE) $ do
            n0m (rCONT <|> rCONC)

sRecord =
    n11 sFamRecord <|>
    n11 sIndividualRecord <|>
    n1m sMultimediaRecord <|>
    n11 sNoteRecord <|>
    n11 sRepositoryRecord <|>
    n11 sSourceRecord <|>
    n11 sSubmitterRecord

sFamRecord =
    when (n11 rFAM) $ do
        each (n01 sFamilyEventStructure) $ do
            when (n01 rHUSB) $ do
                n11 rAGE
            when (n01 rWIFE) $ do
                n11 rAGE
        n01 rHUSB
        n01 rWIFE
        n0m rCHIL
        n01 rNCHI
        n0m rSUBM
        n0m sLdsSpouseSealing
        n0m sSourceCitation
        n0m sMultimediaLink
        n0m sNoteStructure
        each (n01 rREFN) $ do
            n01 rTYPE
        n01 rRIN
        n01 sChangeDate        

sIndividualRecord =
    when (n11 rINDI) $ do
        n01 rRESN
        n0m sPersonalNameStructure
        n01 rSEX
        n0m sIndividualEventStructure
        n0m sIndividualAttributeStructure
        n0m sLdsIndividualOrdinance
        n0m sChildToFamilyLink
        n0m sSpouseToFamilyLink
        n0m rSUBM
        n0m sAssociationStructure
        n0m rALIA
        n0m rANCI
        n0m rDESI
        n0m sSourceCitation
        n0m sMultimediaLink
        n0m sNoteStructure
        n01 rRFN
        n01 rAFN
        each (n01 rREFN) $ do
            n01 rTYPE
        n01 rRIN
        n01 sChangeDate

sMultimediaRecord = xxx

sNoteRecord =
    when (n11 rNOTE) $ do
        n0m (rCONC <|> rCONT)
        n0m sSourceCitation
        each (n01 rREFN) $ do
            n01 rTYPE
        n01 rRIN
        n01 sChangeDate

sRepositoryRecord =
    when (n11 rREPO) $ do
        n01 rNAME
        n01 xWWW
        n01 sAddressStructure
        n0m sNoteStructure
        each (n01 rREFN) $ do
            n01 rTYPE
        n01 rRIN
        n01 sChangeDate

sSourceRecord =
    when (n11 rSOUR) $ do
        when (n01 rDATA) $ do
            each (n01 rEVEN) $ do
                n01 rDATE
                n01 rPLAC
            n01 rAGNC
            n0m sNoteStructure
        when (n01 rAUTH) $ do
            n0m (rCONT <|> rCONC)
        when (n01 rTITL) $ do
            n0m (rCONT <|> rCONC)
        n01 rABBR
        when (n01 rPUBL) $ do
            n0m (rCONT <|> rCONC)
        when (n01 rTEXT) $ do
            n0m (rCONT <|> rCONC)
        n01 sSourceRepositoryCitation
        n0m sMultimediaLink
        n0m sNoteStructure
        each (n01 rREFN) $ do
            n01 rTYPE
        n01 rRIN
        n01 sChangeDate

sSubmissionRecord =
    when (n11 rSUBN) $ do
        n01 rSUBM
        n01 rFAMF
        n01 rTEMP
        n01 rANCE
        n01 rDESC
        n01 rORDI
        n01 rRIN
        
sSubmitterRecord =
    when (n11 rSUBM) $ do
        n11 rNAME
        n01 sAddressStructure
        n0m sMultimediaLink
        n03 rLANG
        n01 rRFN
        n01 rRIN
        n01 sChangeDate



-----------------------------------------------------------------------------
-- substructures, page 29

sAddressStructure = do
    when (n01 rADDR) $ do
        n0m rCONT
        n01 rADR1
        n01 rADR2
        n01 rCITY
        n01 rSTAE
        n01 rPOST
        n01 rCTRY
    n03 rPHON

sAssociationStructure = xxx

sChangeDate =
    when (n11 rCHAN) $ do
        when (n11 rDATE) $ do
            n01 rTIME
        n0m sNoteStructure

sChildToFamilyLink =
    when (n11 rFAMC) $ do
        n0m rPEDI
        n0m sNoteStructure

sEventDetail = do
    n01 rTYPE
    n01 rDATE
    n01 sPlaceStructure
    n01 sAddressStructure
    n01 rAGE
    n01 rAGNC
    n01 rCAUS
    n0m sSourceCitation
    n0m sMultimediaLink
    n0m sNoteStructure

sFamilyEventStructure =
    when (n11 rList) $ do
        n01 sEventDetail
          where
            rList = rANUL <|> rCENS <|> rDIV  <|> rDIVF <|>
                    rENGA <|> rMARR <|> rMARB <|> rMARC <|>
                    rMARL <|> rMARS <|>
                    rEVEN

sIndividualAttributeStructure =
    when (n11 rList) $ do
        n01 sEventDetail
          where
            rList = rCAST <|> rDSCR <|> rEDUC <|> rIDNO <|>
                    rNATI <|> rNCHI <|> rNMR  <|> rOCCU <|>
                    rPROP <|> rRELI <|> rRESI <|> rSSN  <|>
                    rTITL
            
sIndividualEventStructure =
    c1 <|> c2 <|> c3
      where
        c1 = when (n11 $ rBIRT <|> rCHR) $ do
            n01 sEventDetail
            n01 rFAMC

        c2 = when (n11 $ rADOP) $ do
            n01 sEventDetail
            when (n01 rFAMC) $ do
                n01 rADOP
    
        c3 = when (n11 rList) $ do
            n01 sEventDetail
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
        c1 = when (n11 rOBJE) $ do
            n11 rFORM
            n01 rTITL
            n11 rFILE
            n0m sNoteStructure

        c2 = n11 rOBJE

sNoteStructure =
    try c1 <|> try c2
      where
        c1 = when (n11 rNOTE) $ do
            n0m sSourceCitation

        c2 = when (n11 rNOTE) $ do
            n0m (rCONC <|> rCONT)
            n0m sSourceCitation

sPersonalNameStructure =
    when (n11 rNAME) $ do
        n01 rNPFX
        n01 rGIVN
        n01 rNICK
        n01 rSPFX
        n01 rSURN
        n01 rNSFX
        n0m sSourceCitation
        n0m sNoteStructure

sPlaceStructure =
    when (n11 rPLAC) $ do
        when (n01 xMAP) $ do
            n01 xLATI
            n01 xLONG
        n01 rFORM
        n0m sSourceCitation
        n0m sNoteStructure

sSourceCitation =
    try c1 <|> try c2
      where
        c1 = when (n11 rSOUR) $ do
            n01 rPAGE
            when (n01 rEVEN) $ do
                n01 rROLE
            when (n01 rDATA) $ do
                n01 rDATE
                each (n01 rTEXT) $ do
                    n0m (rCONC <|> rCONT)
            n01 rQUAY
            n0m sMultimediaLink
            n0m sNoteStructure

        c2 = when (n11 rSOUR) $ do
            n0m (rCONC <|> rCONT)
            each (n01 rTEXT) $ do
                n0m (rCONC <|> rCONT)
            n0m sNoteStructure

            
sSourceRepositoryCitation =
    when (n11 rREPO) $ do
        n0m sNoteStructure
        each (n01 rCALN) $ do
            n01 rMEDI

sSpouseToFamilyLink =
    when (n11 rFAMS) $ do
        n0m sNoteStructure
        


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
