module Main where

import Text.Printf
import System.Environment
import System.IO.MMap
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import Control.Exception
import HElf.ElfTypes
import HElf.Util

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
    print header
