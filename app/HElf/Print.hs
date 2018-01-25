module HElf.Print where


import HElf.ElfTypes
import HElf.OptParser


printFileHeader :: HElfOptions -> ElfFileHeader -> IO ()
printFileHeader opts fileHeader = do
  if or $ sequenceA [displayAll, displayFileHeader] opts
  then
    print fileHeader
  else
    return ()
