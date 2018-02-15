module HElf.Print where


import Control.Monad (forM, forM_)
import Data.List (zipWith)
import Data.Void (Void)
import Foreign.Ptr (castPtr, plusPtr, Ptr)
import Foreign.C.String
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


printSectionHeaders :: Ptr Void -> HElfOptions -> ElfFileHeader -> IO ()
printSectionHeaders ptr opts fileHeader = do
  let
    doDisplaySectionHeaders = or (sequenceA [displayAll, displaySectionHeaders] opts)
  if doDisplaySectionHeaders
  then do
    let
      pHeaderOffset = fromIntegral (ehSectionHeaderOffset     fileHeader) :: Int
      pHeaderSize   = fromIntegral (ehSectionHeaderEntrySize  fileHeader) :: Int
      numHeaders    = fromIntegral (ehSectionHeaderNumEntries fileHeader) :: Int
      header_offsets = take (numHeaders) (iterate (+pHeaderSize) pHeaderOffset)
      strTabIdx = fromIntegral (ehSectionHeaderStringIndex fileHeader) :: Int
    section_headers <- forM header_offsets (readPtrOffset ptr) :: IO [ElfSectionHeader]
    let
      strTabSection = section_headers !! strTabIdx
      strTabOffset = fromIntegral (eshOffset strTabSection) :: Int
    printSectionHeaderHeadings numHeaders pHeaderOffset
    sequence_ (zipWith (printSectionHeader ptr strTabOffset) [0..] section_headers)
  else
    return ()


printSectionHeaderHeadings :: Int -> Int -> IO ()
printSectionHeaderHeadings numHeaders offset = do
  putStrLn ("There are " ++ (show numHeaders) ++
            " section headers, starting at offset " ++ "0x" ++ (showHex offset "") ++ ":")
  putStr $ unlines [
      "Section Headers:"
    , "  [Nr] Name              Type             Address           Offset"
    , "       Size              EntSize          Flags  Link  Info  Align"
    ]


printSectionHeader :: Ptr Void -> Int -> Int -> ElfSectionHeader -> IO ()
printSectionHeader ptr table_offset num section = do
  let
    string_index = fromIntegral (eshName section) :: Int
    section_num = leftPad 2 ' ' (show num)
    section_type = rightPad 16 ' ' $ showSectionType $ eshType section
    section_address = showHexPadded 16 (eshAddress section)
  section_name <- peekCString (plusPtr ptr (table_offset + string_index))
  let
    padded_section_name = rightPad 16 ' ' section_name
  putStrLn $ unlines [
      "  [" ++ section_num ++ "] " ++ padded_section_name ++ "  " ++ section_type ++ " " ++ section_address
    ]
