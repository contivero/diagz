{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import qualified Data.Binary.Get as G
import Data.Binary.Get (Get)
import qualified Data.Binary.Strict.BitGet as BG
import Options.Applicative hiding (command, header)
import qualified Options.Applicative as OA
import Data.Text
import Data.Monoid ((<>))
import Data.Word (Word8, Word16, Word32)
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Paths_diagz (version)

data Header = Header { signature :: Word16
                     , compressionMethod :: Word8
                     , flags ::  Word8
                     , lastModified :: Word32
                     , extraFlags :: Word8
                     , os :: Word8
                     } deriving (Show)

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

data FlgFextra = FlgFextra { xlen :: Word16
                           , xlenBytes :: ByteString
                           } deriving (Show)

getFlgFextra :: Get FlgFextra
getFlgFextra = do
    xl  <- G.getWord16le
    xlb <- G.getByteString (fromIntegral xl)
    pure $ FlgFextra xl xlb

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

operatingSystem :: Word8 -> Text
operatingSystem 0   = "FAT filesystem (MS-DOS, OS/2, NT/Win32)"
operatingSystem 1   = "Amiga"
operatingSystem 2   = "VMS (or OpenVMS)"
operatingSystem 3   = "UNIX"
operatingSystem 4   = "VM/CMS"
operatingSystem 5   = "Atari TOS"
operatingSystem 6   = "HPFS filesystem (OS/2, NT)"
operatingSystem 7   = "Macintosh"
operatingSystem 8   = "Z-System"
operatingSystem 9   = "CP/M"
operatingSystem 10  = "TOPS-20"
operatingSystem 11  = "NTFS filesystem (NT)"
operatingSystem 12  = "QDOS"
operatingSystem 13  = "Acorn RISCOS"
operatingSystem 255 = "unknown"
operatingSystem x   = "undefined (" <> pack (show x) <> ")"

cmethod :: Word8 -> Text
cmethod 8 = "DEFLATE"
cmethod x = "reserved (" <> pack (show x) <> ")"

eflags :: Word8 -> Text
eflags 0x02 = "maximum compression (slowest algorithm)"
eflags 0x04 = "fastest algorithm"
eflags x    = "undefined (" <> pack (show x) <> ")"

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

f :: Get Flags
f = do
    fla <- G.getByteString 1
    let x = BG.runBitGet fla $ do
            ftext'    <- BG.getBit
            fhcrc'    <- BG.getBit
            fextra'   <- BG.getBit
            fname'    <- BG.getBit
            fcomment' <- BG.getBit
            r0x20'    <- BG.getBit
            r0x40'    <- BG.getBit
            r0x80'    <- BG.getBit
            pure $ Flags ftext' fhcrc' fextra' fname' fcomment' r0x20' r0x40' r0x80'
    case x of
      Left error -> fail error
      Right r -> return r


deserializeHeader :: Get Header
deserializeHeader = do
    sig <- G.getWord16le
    cm  <- G.getWord8
    flg <- G.getWord8
    lm  <- G.getWord32le
    efg <- G.getWord8
    o   <- G.getWord8
    pure $ Header sig cm flg lm efg o

data Commands = Commands { filePath :: FilePath }
  deriving (Show)

commands :: Parser Commands
commands = Commands <$> argument str (metavar "FILE")

instructions :: ParserInfo Commands
instructions = info (helper <*> versionOption <*>  commands) --  ((,) <$> command <*> config))
    (fullDesc <> OA.header "Diagz - Diagnostics for gzip")
  where versionOption = infoOption (showVersion version <> " " <> $(gitHash))
                                   (long "version" <> help "Show version and commit hash")

main :: IO ()
main = do
    cmds  <- execParser instructions
    input <- BL.readFile (filePath cmds)
    let h = G.runGet deserializeHeader input
    print h
    let l = posixSecondsToUTCTime (fromRational . toRational . secondsToDiffTime  $ toInteger (lastModified h) :: NominalDiffTime)
        i = compressionMethod h
        o = os h
        e = extraFlags h
        x = G.runGet f $ BL.singleton (flags h)
    print $ cmethod i
    print x
    print l
    print $ eflags e
    print $ operatingSystem o

