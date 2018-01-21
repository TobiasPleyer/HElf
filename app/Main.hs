module Main where

import Text.Printf
import System.Environment
import System.IO.MMap
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import Control.Exception
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
  putStrLn "--------------------------"
  putStrLn $ "File: " ++ filename
  putStrLn "--------------------------"
  header <- bracket
    (mmapFilePtr filename ReadOnly (Just (0, 64)))
    (\(ptr,rawsize,_,_) -> munmapFilePtr ptr rawsize)
    (\(ptr,_,_,_) -> peek ptr :: IO ElfHeader)
  if not (verifyElf (ehIdentification header))
  then
    printf "\nNot an ELF file!\n"
  else
    print header
