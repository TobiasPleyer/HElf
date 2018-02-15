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
import System.Exit (die)
import HElf.ElfTypes
import HElf.Util
import HElf.OptParser
import HElf.Print


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
  if (not . or) $ sequenceA [ displayAll
                            , displayFileHeader
                            , displayProgramHeaders
                            , displaySectionHeaders
                            , displaySectionGroups
                            , displaySectionDetails
                            , displayHeaders
                            , displaySymbolTable
                            , displayDynamicSymbolTable
                            , displayCoreNotes
                            , displayRelocations
                            , displayUnwindInfo
                            , displayDynamicSection
                            , displayVersionSections
                            , displayHElfVersion] options
  then
    die "No options!"
  else
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
  assertElf fileHeader
  printFileHeader opts fileHeader
  printProgramHeaders ptr opts fileHeader
  printSectionHeaders ptr opts fileHeader
