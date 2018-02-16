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
    printFlagKeys
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
    section_size = showHexPadded 16 (eshSize section)
    section_entrysize = showHexPadded 16 (eshEntrySize section)
    section_flags = ((rightPad 8 ' ') . showSectionFlags . eshFlags) section
    section_link = ((rightPad 5 ' ') . show . eshLink) section
    section_info = ((rightPad 5 ' ') . show . eshInfo) section
    section_alignment = ((rightPad 5 ' ') . show . eshAlignment) section
    section_offset = showHexPadded 8 (eshOffset section)
  putStr $ unlines [
      "  [" ++ section_num ++ "] " ++ padded_section_name ++ "  " ++
      section_type ++ " " ++ section_address ++ "  " ++ section_offset
    , "       " ++ section_size ++ "  " ++ section_entrysize ++ " " ++
      section_flags ++ " " ++ section_link ++
      " " ++ section_info ++ " " ++ section_alignment
    ]


printFlagKeys :: IO ()
printFlagKeys = do
  putStr $ unlines [
      "Key to Flags:"
    , "  W (write), A (alloc), X (execute), M (merge), S (strings), l (large)"
    , "  I (info), L (link order), G (group), T (TLS), E (exclude), x (unknown)"
    , "  O (extra OS processing required) o (OS specific), p (processor specific)"
    ]
