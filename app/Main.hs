module Main where

import Text.Printf
import System.Environment
import System.IO.MMap
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import ElfTypes
import Lib
import Control.Exception

main :: IO ()
main = do
  filename <- head <$> getArgs
  header <- bracket
    (mmapFilePtr filename ReadOnly (Just (0, 64)))
    (\(ptr,rawsize,_,_) -> munmapFilePtr ptr rawsize)
    (\(ptr,_,_,_) -> peek ptr :: IO ElfHeader)
  if not (verifyElf (ehIdentification header))
  then
    printf "\nNot an ELF file!\n"
  else
    printf "\nThe entry address of %s is 0x%lx\n" filename (fromIntegral (ehEntryPoint header) :: Int)
  print header
