{-# LANGUAGE OverloadedStrings#-}

module Diagz where

import Data.Binary.Get (Get)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Word
import Numeric (showHex)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString.Lazy as BL
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import Data.Time.Clock.POSIX
import Data.Time.Clock

data Header = Header { id1 :: Word8
                     , id2 :: Word8
                     , compressionMethod :: Word8
                     , flags ::  Word8
                     , lastModified :: Word32
                     , extraFlags :: Word8
                     , operatingSystem :: Word8
                     } deriving (Show)

instance Pretty Header where
  pretty (Header i1 i2 cm fs lm ef os) =
      "Header (10 bytes)" <$$> indent 2 (
      fill m (text t1) <> colon <+> text (show i1) <+> text (show i2) <$$>
      fill m (text t2) <> colon <+> cmethod cm <$$>
      fill m (text t3) <> colon <+> hang 2 (text (show fs) <$$> pretty (G.runGet getFlags (BL.singleton fs))) <$$>
      fill m (text t4) <> colon <+> timeToDoc lm <$$>
      fill m (text t5) <> colon <+> eflags ef <$$>
      fill m (text t6) <> colon <+> osToDoc os  <> linebreak)
    where a@[t1,t2,t3,t4,t5,t6] = 
                [ "Signature", "Compression Method", "Flags"
                , "Last Modified", "Extra Flags", "Operating System"]
          m = (maximum $ map length a) + 1

instance Pretty Word8 where
  pretty x = text (show x)

-- | The 8 bits corresponding to the 'Header' flags (FLG in IRC 1952)
data Flags = Flags { ftext :: Bool
                   , fhcrc :: Bool
                   , fextra :: Bool
                   , fname :: Bool
                   , fcomment :: Bool
                   , r0x20 :: Bool
                   , r0x40 :: Bool
                   , r0x80 :: Bool
                   } deriving (Show)
instance Pretty Flags where
  pretty (Flags ft fh fe fn fc r2 r4 r8) = 
      align (fill m (text t1) <+> equals <+> bitToDoc ft
        <$$> fill m (text t2) <+> equals <+> bitToDoc fh
        <$$> fill m (text t3) <+> equals <+> bitToDoc fe
        <$$> fill m (text t4) <+> equals <+> bitToDoc fn
        <$$> fill m (text t5) <+> equals <+> bitToDoc fc)
    where a@[t1,t2,t3,t4,t5] = ["FTEXT", "FHCRC", "FEXTRA" , "FNAME", "FCOMMENT"]
          m = (maximum $ map length a) 

data FlgFextra = FlgFextra { xlen :: Word16
                           , xlenBytes :: ByteString
                           } deriving (Show)

data OptionalHeader = OptionalHeader { flg_fextra  :: Maybe FlgFextra
                                     , fileName    :: Maybe ByteString
                                     , fileComment :: Maybe ByteString
                                     , crc16       :: Maybe Word16
                                     } deriving (Show)

data Member = Member { header           :: Header
                     , optionalHeader   :: OptionalHeader
                     , compressedBlocks :: ByteString
                     , crc32            :: Word32
                     , inputSize        :: Word32
                     } deriving (Show)

data Footer = Footer { checksum :: Word32 -- Checksum (CRC-32)
                     , dataSize :: Word32 -- Uncompressed data size (in bytes).
                     }

osToDoc :: Word8 -> Doc
osToDoc 0   = "FAT filesystem (MS-DOS, OS/2, NT/Win32)"
osToDoc 1   = "Amiga"
osToDoc 2   = "VMS (or OpenVMS)"
osToDoc 3   = "UNIX"
osToDoc 4   = "VM/CMS"
osToDoc 5   = "Atari TOS"
osToDoc 6   = "HPFS filesystem (OS/2, NT)"
osToDoc 7   = "Macintosh"
osToDoc 8   = "Z-System"
osToDoc 9   = "CP/M"
osToDoc 10  = "TOPS-20"
osToDoc 11  = "NTFS filesystem (NT)"
osToDoc 12  = "QDOS"
osToDoc 13  = "Acorn RISCOS"
osToDoc 255 = "Unknown"
osToDoc x   = "Undefined" <+> parens (text (show x))

cmethod :: Word8 -> Doc
cmethod 8 = text "DEFLATE" <+> parens (int 8)
cmethod x = text "Reserved" <+> parens (text (show x))

eflags :: Word8 -> Doc
eflags 2 = "Maximum compression, slowest algorithm" <+> parens (int 2)
eflags 4 = "Fastest algorithm" <+> parens (int 4)
eflags x = "Undefined" <+> parens (int $ fromIntegral x)

{-
Value	Identifier	Description
0x01	FTEXT	If set the uncompressed data needs to be treated as text instead of binary data.
This flag hints end-of-line conversion for cross-platform text files but does not enforce it.
0x02	FHCRC	The file contains a header checksum (CRC-16)
0x04	FEXTRA	The file contains extra fields
0x08	FNAME	The file contains an original file name string
0x10	FCOMMENT	The file contains comment
0x20		Reserved
0x40		Reserved
0x80		Reserved
-}

flgs :: Word8 -> Text
flgs 0x01 = "FTEXT If set the uncompressed data needs to be treated as text instead of binary data."
flgs 0x02 = "FHCRC The file contains a header checksum (CRC-16)"
flgs 0x04 = "FEXTRA The file contains extra fields"
flgs 0x08 = "FNAME The file contains an original file name string"
flgs 0x10 = "FCOMMENT The file contains comment"
flgs 0x20 = "Reserved (0x20)"
flgs 0x40 = "Reserved (0x40)"
flgs 0x80 = "Reserved (0x80)"

getFlags :: Get Flags
getFlags = do
    flg <- G.getByteString 1
    -- The monad gets bits from left to right, so the fields are backwards.
    let x = BG.runBitGet flg $ do
            r0x80'    <- BG.getBit
            r0x40'    <- BG.getBit
            r0x20'    <- BG.getBit
            fcomment' <- BG.getBit
            fname'    <- BG.getBit
            fextra'   <- BG.getBit
            fhcrc'    <- BG.getBit
            ftext'    <- BG.getBit
            pure $ Flags ftext' fhcrc' fextra' fname' fcomment' r0x20' r0x40' r0x80'
    case x of
      Left error -> fail error
      Right r -> return r

-- TODO: Make it a Get Member, with all it entails.
deserializeMember :: Get (Header, ByteString)
deserializeMember = do
    h  <- deserializeHeader
    fn <- getFileName
    pure $ (h, fn)

deserializeHeader :: Get Header
deserializeHeader = do  
    i1  <- G.getWord8 
    i2  <- G.getWord8 
    cm  <- G.getWord8
    flg <- G.getWord8
    lm  <- G.getWord32le
    efg <- G.getWord8
    o   <- G.getWord8
    pure $ Header i1 i2 cm flg lm efg o

getFlgFextra :: Get FlgFextra
getFlgFextra = do
    xl  <- G.getWord16le
    xlb <- G.getByteString (fromIntegral xl)
    pure $ FlgFextra xl xlb

getFileName :: Get ByteString
getFileName = do
    fn <- G.getLazyByteStringNul
    pure $ BL.toStrict fn

-- flgfextra (Header _ _ _ flg _ _ _) = 
  -- where flgfextra' (Flags _ _ _ 

signatureIsCorrect :: Word8 -> Word8 -> Bool
signatureIsCorrect i1 i2 = i1 == 31 && i2 == 139

timeToDoc :: Word32 -> Doc
timeToDoc x = text . show $ posixSecondsToUTCTime posixSecs
  where posixSecs :: NominalDiffTime
        posixSecs = fromRational . toRational . secondsToDiffTime $ toInteger x 

bitToDoc :: Bool -> Doc
bitToDoc False = int 0
bitToDoc True  = int 1
