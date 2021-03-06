module HElf.Util where

import Data.Array
import Foreign.C.Types (CUChar)
import Foreign.Ptr (castPtr, plusPtr, Ptr)
import Foreign.Storable (Storable(..))
import Data.List (intercalate)
import Data.Void
import Numeric (showHex)
import System.Exit (die)
import HElf.ElfTypes


hasElfMagic :: Array Int CUChar -> Bool
hasElfMagic arr =
  (arr ! 0 == 0x7F) &&
  (arr ! 1 == 0x45) &&
  (arr ! 2 == 0x4c) &&
  (arr ! 3 == 0x46)


verifyElf :: ElfFileHeader -> Bool
verifyElf = hasElfMagic . ehIdentification


assertElf :: ElfFileHeader -> IO ()
assertElf fileHeader = do
  if not (verifyElf fileHeader)
  then
    die "helf: Error: Not an ELF file - it has the wrong magic bytes at the start"
  else
    return ()


readPtrOffset :: Storable a => Ptr Void -> Int -> IO a
readPtrOffset p o = peek (plusPtr p o)
