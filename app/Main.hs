module Main where

import System.IO.MMap (
      Mode(..)
    , mmapFilePtr
    , munmapFilePtr
    )
import Foreign.Ptr (castPtr, Ptr)
import Foreign.Storable (peek)
import Control.Exception (bracket)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Foldable (forM_)
import Data.Void (Void)
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
  bracket
    (mmapFilePtr filename ReadOnly Nothing)
    (\(ptr,rawsize,_,_) -> munmapFilePtr ptr rawsize)
    (\(ptr,_,_,_) -> readFromPtr ptr options)


readFromPtr :: Ptr Void -> HElfOptions -> IO ()
readFromPtr ptr opts = do
  fileHeader <- peek (castPtr ptr) :: IO ElfFileHeader
  if not (verifyElf fileHeader)
  then
    putStrLn "Not an ELF file!"
  else
    return ()
  if or [displayAll opts, displayFileHeader opts]
  then
    print fileHeader
  else
    return ()
