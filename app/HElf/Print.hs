module HElf.Print where


import Control.Monad (forM, forM_)
import Data.Void (Void)
import Foreign.Ptr (castPtr, Ptr)
import Numeric (showHex)
import HElf.ElfTypes
import HElf.OptParser
import HElf.Show
import HElf.Util


printFileHeader :: HElfOptions -> ElfFileHeader -> IO ()
printFileHeader opts fileHeader = do
  if (or (sequenceA [displayAll, displayFileHeader] opts))
  then
    print fileHeader
  else
    return ()


printProgramHeaders :: Ptr Void -> HElfOptions -> ElfFileHeader -> IO ()
printProgramHeaders ptr opts fileHeader = do
  let
    doDisplayFileHeader = or (sequenceA [displayAll, displayFileHeader] opts)
    doDisplayProgramHeaders = or (sequenceA [displayAll, displayProgramHeaders] opts)
  -- If the ELF file header is not displayed, then we need a bit extra info
  if ((not doDisplayFileHeader) && doDisplayProgramHeaders)
  then do
    let
      fileType = showElfType (ehObjectType fileHeader)
      entryPoint = (showHex (ehEntryPoint fileHeader) "")
      numHeaders = show (ehProgramHeaderNumEntries fileHeader)
      offsetHeaders = show (ehProgramHeaderOffset fileHeader)
    putStrLn $ unlines [
        "Elf file type is " ++ fileType
      , "Entry point " ++ "0x" ++ entryPoint
      , "There are " ++ numHeaders ++ " program headers, starting at offset " ++ offsetHeaders
      ]
  else
    return ()

  if doDisplayProgramHeaders
  then do
    printProgramHeaderHeadings
    let
      pHeaderOffset = fromIntegral (ehProgramHeaderOffset     fileHeader) :: Int
      pHeaderSize   = fromIntegral (ehProgramHeaderEntrySize  fileHeader) :: Int
      numHeaders    = fromIntegral (ehProgramHeaderNumEntries fileHeader) :: Int
      header_offsets = take (numHeaders) (iterate (+pHeaderSize) pHeaderOffset)
    program_headers <- forM header_offsets (readPtrOffset ptr) :: IO [ElfProgramHeader]
    forM_ program_headers (putStr . show)
  else
    return ()


printProgramHeaderHeadings :: IO ()
printProgramHeaderHeadings =
  putStr $ unlines [
      "Program Headers:"
    , "  Type           Offset             VirtualAddress     PhysicalAddress"
    , "                 FileSize           MemorySize         Flags   Align"
    ]
