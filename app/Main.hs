module Main where

import System.IO.MMap (
      Mode(..)
    , mmapFilePtr
    , munmapFilePtr
    )
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import Control.Exception (bracket)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (forM_)
import HElf.ElfTypes
import HElf.Util
import HElf.OptParser


opts = info (optparser <**> helper)
  (  fullDesc
  <> progDesc "Display information about the contents of ELF format files"
  <> header "HElf - a readelf clone based on Haskell" )


main :: IO ()
main = do
  options <- execParser opts
  forM_ (filenames options) (displayFileInfo options)


displayFileInfo :: HElfOptions -> String -> IO ()
displayFileInfo options filename = do
  (putStrLn. unlines) [
    "--------------------------",
    "File: " ++ filename,
    "--------------------------"]
  header <- readHeader filename
  if not (verifyElf header)
  then
    putStrLn "Not an ELF file!"
  else
    print header


verifyElf :: ElfFileHeader -> Bool
verifyElf = hasElfMagic . ehIdentification


readHeader :: FilePath -> IO ElfFileHeader
readHeader f = do
  bracket
    (mmapFilePtr f ReadOnly (Just (0, 64)))
    (\(ptr,rawsize,_,_) -> munmapFilePtr ptr rawsize)
    (\(ptr,_,_,_) -> peek ptr :: IO ElfFileHeader)
