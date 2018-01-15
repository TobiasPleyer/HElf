module Main where

import Text.Printf
import System.Environment
import System.IO.MMap
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import ElfTypes

main :: IO ()
main = do
  filename <- head <$> getArgs
  (ptr,_,_,_) <- mmapFilePtr filename ReadOnly (Just (0, 64))
  header <- peek ptr :: IO ElfHeader
  printf "The entry address of %s is 0x%lx\n" filename (fromIntegral (ehEntryPoint header) :: Int)
