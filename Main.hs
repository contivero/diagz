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
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
import Data.Text
import Data.Monoid ((<>))
import Data.Word (Word8, Word16, Word32)
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Paths_diagz (version)

import Diagz

data Commands = Commands { filePath :: FilePath }
  deriving (Show)

commands :: Parser Commands
commands = Commands <$> argument str (metavar "FILE")

instructions :: ParserInfo Commands
instructions = info (helper <*> versionOption <*>  commands) --  ((,) <$> command <*> config))
    (fullDesc <> OA.header "Diagz - Diagnostics for gzip")
  where versionOption = infoOption ("Diagz version: " <> showVersion version <> " " <> $(gitHash))
                                   (long "version" <> help "Show version and commit hash")

main :: IO ()
main = do
    cmds  <- execParser instructions
    input <- BL.readFile (filePath cmds)
    memb input

-- member
memb :: BL.ByteString -> IO ()
memb input = do
    let m = G.runGet deserializeMember input
    putDoc $ pretty m <> linebreak

-- deflate
def :: BL.ByteString -> IO ()
def input = do
    let m = G.runGet deserializeDeflateHeader input
    putDoc $ pretty m <> linebreak
