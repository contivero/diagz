{-# LANGUAGE OverloadedStrings#-}

-----------------------------------------------------------------------------
-- |
-- Module      : Diagz
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------

module Diagz where

import Control.Applicative ((<**>))
import Data.Binary.Get (Get)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Monoid ((<>))
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Word
import Data.Bits (testBit)
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
      fill m (text t3) <> colon <+> (text (show fs) <$$> indent 4 (flagBreakdown fs)) <$$>
      fill m (text t4) <> colon <+> timeToDoc lm <$$>
      fill m (text t5) <> colon <+> eflags ef <$$>
      fill m (text t6) <> colon <+> osToDoc os)
    where a@[t1,t2,t3,t4,t5,t6] = 
                [ "Signature", "Compression Method", "Flag Byte Value"
                , "Last Modified", "Extra Flags Value", "Operating System"]
          m = (maximum $ map length a) + 1

instance Pretty Word8 where
  pretty x = text (show x)

flagBreakdown :: Word8 -> Doc
flagBreakdown x = "Flag Breakdown" <+> colon <+>
    align (fill m (text t0) <+> equals <+> bitToDoc (ftext x)
      <$$> fill m (text t1) <+> equals <+> bitToDoc (fhcrc x)
      <$$> fill m (text t2) <+> equals <+> bitToDoc (fextra x)
      <$$> fill m (text t3) <+> equals <+> bitToDoc (fname x)
      <$$> fill m (text t4) <+> equals <+> bitToDoc (fcomment x)
      <$$> fill m (text t5) <+> equals <+> bitToDoc (r0x20 x)
      <$$> fill m (text t6) <+> equals <+> bitToDoc (r0x40 x)
      <$$> fill m (text t7) <+> equals <+> bitToDoc (r0x80 x))
  where a@[t0,t1,t2,t3,t4,t5,t6,t7] =
          [ "Bit 0 (FTEXT)", "Bit 1 (FHCRC)", "Bit 2 (FEXTRA)"
          , "Bit 3 (FNAME)", "Bit 4 (FCOMMENT)", "Bit 5 (reserved)"
          , "Bit 6 (reserved)", "Bit 7 (reserved)"
          ]
        m = (maximum $ map length a) 

data FlgFextra = FlgFextra { xlen :: Word16
                           , xlenBytes :: ByteString
                           } deriving (Show)

data Member = Member { header           :: Header
                     , flg_fextra       :: Maybe FlgFextra
                     , fileName         :: Maybe ByteString
                     , fileComment      :: Maybe ByteString
                     , crc16            :: Maybe Word16
                     -- , compressedBlocks :: ByteString
                     , deflateHeader    :: DeflateHeader
                     , crc32            :: Word32
                     , inputSize        :: Word32
                     } deriving (Show)
instance Pretty Member where
  pretty (Member h ffe fn fc crc_16 dh crc_32 isz) =
      pretty h <$$> (if optionalHeaderIsPresent ffe fn fc crc_16
                      then "Optional Header" <$$> indent 2 (prettyFileName)
                      else mempty) <$$>
      "CRC16" <> colon <+> text (show crc_16) <$$>
      pretty dh
    where prettyFileName = maybe mempty (\x -> "File name" <> colon <+> (text $ C.unpack x)) fn

data DeflateHeader = DeflateHeader { bfinal :: Bfinal
                                   , btype  :: Btype
                                   } deriving Show
data Bfinal = NotLastBlock | LastBlock
  deriving Show
instance Pretty Bfinal where
  pretty NotLastBlock = "0 (Not the last block of the data set)"
  pretty LastBlock    = "1 (Last block of the data set)"

data Btype = NoCompression
           | FixedHuffman
           | DynamicHuffman
           | Reserved
  deriving Show
instance Pretty Btype where
  pretty NoCompression  = "00 (No compression)"
  pretty FixedHuffman   = "01 (Compressed with fixed huffman codes)"
  pretty DynamicHuffman = "10 (Compressed with dynamic huffman codes)"
  pretty Reserved       = "11 (Reserved; error)"

instance Pretty DeflateHeader where
  pretty (DeflateHeader bf bt) =
      "Deflate Header" <$$> indent 2 (align (
        fill m (text t0) <> colon <+> pretty bf <$$> 
        fill m (text t1) <> colon <+> pretty bt))
    where a@[t0,t1] =
            [ "Final Block (BFINAL)", "Block Type (BTYPE)"]
          m = (maximum $ map length a) + 1

optionalHeaderIsPresent :: Maybe FlgFextra -> Maybe ByteString
                        -> Maybe ByteString -> Maybe Word16 -> Bool
optionalHeaderIsPresent a b c d
    | isJust a || isJust b || isJust c || isJust d = True
    | otherwise                                    = False

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
eflags 2 = int 2 <+> parens "Maximum compression, slowest algorithm"
eflags 4 = int 4 <+> parens "Fastest algorithm"
eflags x = (int $ fromIntegral x) <+> parens "Undefined"

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

deserializeMember :: Get Member
deserializeMember = do
    h  <- deserializeHeader
    if compressionMethod h /= 8
       then error "Unkown compression method, aborting."
       else do let flgs = flags h
               fe <- if fextra flgs
                        then Just <$> getFlgFextra
                        else pure Nothing
               fn <- if fname flgs
                        then getNullTerminatedString  
                        else pure Nothing
               fc <- if fcomment flgs
                        then getNullTerminatedString
                        else pure Nothing
               c  <- if fhcrc flgs
                        then Just <$> G.getWord16le
                        else pure Nothing
               dh <- deserializeDeflateHeader
               pure $ Member h fe fn fc c dh 0 0

deserializeDeflateHeader :: Get DeflateHeader
deserializeDeflateHeader = do
    x <- G.getByteString 1
    let y = BG.runBitGet x $ do
            b0 <- BG.getBit <**> pure (\x -> if x then LastBlock else NotLastBlock)
            b1 <- BG.getBit
            b2 <- BG.getBit
            pure $ DeflateHeader b0 (mkBtype b2 b1)
    case y of
      Right a -> pure a
      Left err -> fail err
  where mkBtype False False = NoCompression
        mkBtype False True  = FixedHuffman
        mkBtype True False  = DynamicHuffman
        mkBtype True True   = Reserved

getNullTerminatedString :: Get (Maybe ByteString)
getNullTerminatedString = do
 x <- G.getLazyByteStringNul
 pure . Just $ BL.toStrict x

deserializeHeader :: Get Header
deserializeHeader = do  
    i1  <- G.getWord8 
    i2  <- G.getWord8 
    cm  <- G.getWord8
    flg <- G.getWord8
    lm  <- G.getWord32le
    efg <- G.getWord8
    o   <- G.getWord8
    if signatureIsValid i1 i2
       then pure $ Header i1 i2 cm flg lm efg o
       else error "Invalid gzip signature. Aborting."

getFlgFextra :: Get FlgFextra
getFlgFextra = do
    xl  <- G.getWord16le
    xlb <- G.getByteString (fromIntegral xl)
    pure $ FlgFextra xl xlb

signatureIsValid :: Word8 -> Word8 -> Bool
signatureIsValid = \i1 i2 -> i1 == 31 && i2 == 139

timeToDoc :: Word32 -> Doc
timeToDoc 0 = "No time stamp available (0)"
timeToDoc x = text . show $ posixSecondsToUTCTime posixSecs
  where posixSecs :: NominalDiffTime
        posixSecs = fromRational . toRational . secondsToDiffTime $ toInteger x 

bitToDoc :: Bool -> Doc
bitToDoc False = int 0
bitToDoc True  = int 1

ftext :: Word8 -> Bool
ftext = flip testBit 0

fhcrc :: Word8 -> Bool
fhcrc = flip testBit 1

fextra :: Word8 -> Bool
fextra = flip testBit 2

fname :: Word8 -> Bool
fname = flip testBit 3

fcomment :: Word8 -> Bool
fcomment = flip testBit 4

r0x20 :: Word8 -> Bool
r0x20 = flip testBit 5

r0x40 :: Word8 -> Bool
r0x40 = flip testBit 6 

r0x80 :: Word8 -> Bool
r0x80 = flip testBit 7
