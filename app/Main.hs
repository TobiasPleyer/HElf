module Main where

import Text.Printf
import System.Environment
import System.IO.MMap
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import ElfTypes
import Lib

main :: IO ()
main = do
  filename <- head <$> getArgs
  (ptr,_,_,_) <- mmapFilePtr filename ReadOnly (Just (0, 64))
  header <- peek ptr :: IO ElfHeader
  let isHeader = verifyElf (ehIdentification header)
  if not isHeader
  then
    printf "\nNot an ELF file!\n"
  else
    printf "\nThe entry address of %s is 0x%lx\n" filename (fromIntegral (ehEntryPoint header) :: Int)
  print header
