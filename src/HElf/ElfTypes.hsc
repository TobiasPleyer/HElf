{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}


module HElf.ElfTypes where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Data.Array
import Numeric (showHex)
import HElf.Show


#include "ElfStructs64.h"


data ElfFileHeader = ElfFileHeader
  { ehIdentification           :: Array Int CUChar
  , ehObjectType               :: CUShort
  , ehMachineType              :: CUShort
  , ehVersion                  :: CUInt
  , ehEntryPoint               :: CULong
  , ehProgramHeaderOffset      :: CULong
  , ehSectionHeaderOffset      :: CULong
  , ehFlags                    :: CUInt
  , ehHeaderSize               :: CUShort
  , ehProgramHeaderEntrySize   :: CUShort
  , ehProgramHeaderNumEntries  :: CUShort
  , ehSectionHeaderEntrySize   :: CUShort
  , ehSectionHeaderNumEntries  :: CUShort
  , ehSectionHeaderStringIndex :: CUShort
  } deriving (Eq)


instance Storable ElfFileHeader where
  alignment _ = #{alignment ElfFileHeader_t}
  sizeOf _    = #{size      ElfFileHeader_t}
  peek p =
    ElfFileHeader <$> (listArray (0,15) <$> (peekArray 16 (castPtr p) :: IO [CUChar]))
                  <*> #{peek ElfFileHeader_t, e_type     } p
                  <*> #{peek ElfFileHeader_t, e_machine  } p
                  <*> #{peek ElfFileHeader_t, e_version  } p
                  <*> #{peek ElfFileHeader_t, e_entry    } p
                  <*> #{peek ElfFileHeader_t, e_phoff    } p
                  <*> #{peek ElfFileHeader_t, e_shoff    } p
                  <*> #{peek ElfFileHeader_t, e_flags    } p
                  <*> #{peek ElfFileHeader_t, e_ehsize   } p
                  <*> #{peek ElfFileHeader_t, e_phentsize} p
                  <*> #{peek ElfFileHeader_t, e_phnum    } p
                  <*> #{peek ElfFileHeader_t, e_shentsize} p
                  <*> #{peek ElfFileHeader_t, e_shnum    } p
                  <*> #{peek ElfFileHeader_t, e_shstrndx } p
  poke p ElfFileHeader{..} = do
                  (pokeArray (castPtr p) (elems ehIdentification))
                  #{poke ElfFileHeader_t, e_type     } p ehObjectType
                  #{poke ElfFileHeader_t, e_machine  } p ehMachineType
                  #{poke ElfFileHeader_t, e_version  } p ehVersion
                  #{poke ElfFileHeader_t, e_entry    } p ehEntryPoint
                  #{poke ElfFileHeader_t, e_phoff    } p ehProgramHeaderOffset
                  #{poke ElfFileHeader_t, e_shoff    } p ehSectionHeaderOffset
                  #{poke ElfFileHeader_t, e_flags    } p ehFlags
                  #{poke ElfFileHeader_t, e_ehsize   } p ehHeaderSize
                  #{poke ElfFileHeader_t, e_phentsize} p ehProgramHeaderEntrySize
                  #{poke ElfFileHeader_t, e_phnum    } p ehProgramHeaderNumEntries
                  #{poke ElfFileHeader_t, e_shentsize} p ehSectionHeaderEntrySize
                  #{poke ElfFileHeader_t, e_shnum    } p ehSectionHeaderNumEntries
                  #{poke ElfFileHeader_t, e_shstrndx } p ehSectionHeaderStringIndex

instance Show ElfFileHeader where
  show ElfFileHeader{..} = unlines [
      "ELF Header:"
    , "  Magic:   " ++ create_magic ehIdentification
    , "  Class:                             " ++ showElfClass (ehIdentification ! 4)
    , "  Data:                              " ++ showElfData (ehIdentification ! 5)
    , "  Version:                           " ++ show (ehIdentification ! 6) ++ " (current)"
    , "  OS/ABI:                            " ++ showElfOSABI (ehIdentification ! 7)
    , "  ABI Version:                       " ++ show (ehIdentification ! 8)
    , "  Type:                              " ++ showElfType ehObjectType
    , "  Machine:                           " ++ showElfMachineType ehMachineType
    , "  Version:                           " ++ show ehVersion
    , "  Entry point address:               " ++ "0x" ++ (showHex ehEntryPoint "")
    , "  Start of program headers:          " ++ show ehProgramHeaderOffset ++ " (bytes into file)"
    , "  Start of section headers:          " ++ show ehSectionHeaderOffset ++ " (bytes into file)"
    , "  Flags:                             " ++ "0x" ++ showHex ehFlags ""
    , "  Size of this header:               " ++ show ehHeaderSize ++ " (bytes)"
    , "  Size of program headers:           " ++ show ehProgramHeaderEntrySize ++ " (bytes)"
    , "  Number of program headers:         " ++ show ehProgramHeaderNumEntries
    , "  Size of section headers:           " ++ show ehSectionHeaderEntrySize ++ " (bytes)"
    , "  Number of section headers:         " ++ show ehSectionHeaderNumEntries
    , "  Section header string table index: " ++ show ehSectionHeaderStringIndex
    ]

  showsPrec d h = (\s -> s)


data ElfProgramHeader = ElfProgramHeader
 { ephType            :: CUInt
 , ephFlags           :: CUInt
 , ephOffset          :: CULong
 , ephVirtualAddress  :: CULong
 , ephPhysicalAddress :: CULong
 , ephFileSize        :: CULong
 , ephMemorySize      :: CULong
 , ephAlignment       :: CULong
 } deriving (Eq)


instance Storable ElfProgramHeader where
  alignment _ = #{alignment ElfProgramHeader_t}
  sizeOf _    = #{size      ElfProgramHeader_t}
  peek p =
    ElfProgramHeader <$> #{peek ElfProgramHeader_t, p_type  } p
                     <*> #{peek ElfProgramHeader_t, p_flags } p
                     <*> #{peek ElfProgramHeader_t, p_offset} p
                     <*> #{peek ElfProgramHeader_t, p_vaddr } p
                     <*> #{peek ElfProgramHeader_t, p_paddr } p
                     <*> #{peek ElfProgramHeader_t, p_filesz} p
                     <*> #{peek ElfProgramHeader_t, p_memsz } p
                     <*> #{peek ElfProgramHeader_t, p_align } p

  poke p ElfProgramHeader{..} = do
                     #{poke ElfProgramHeader_t, p_type  } p ephType
                     #{poke ElfProgramHeader_t, p_flags } p ephFlags
                     #{poke ElfProgramHeader_t, p_offset} p ephOffset
                     #{poke ElfProgramHeader_t, p_vaddr } p ephVirtualAddress
                     #{poke ElfProgramHeader_t, p_paddr } p ephPhysicalAddress
                     #{poke ElfProgramHeader_t, p_filesz} p ephFileSize
                     #{poke ElfProgramHeader_t, p_memsz } p ephMemorySize
                     #{poke ElfProgramHeader_t, p_align } p ephAlignment

instance Show ElfProgramHeader where
  show ElfProgramHeader{..} = unlines [
      "  " ++ (rightPad 15 ' ' (showProgramHeaderType ephType)) ++ "0x" ++
      (showHexPadded 16 ephOffset) ++ " 0x" ++ (showHexPadded 16 ephVirtualAddress)
      ++ " 0x" ++ (showHexPadded 16 ephPhysicalAddress)
    , "  " ++ replicate 15 ' ' ++ "0x" ++ (showHexPadded 16 ephFileSize) ++ " 0x" ++
      (showHexPadded 16 ephMemorySize) ++ " " ++ (showProgramHeaderFlags ephFlags)
      ++ (rightPad 10 ' ' (showHex ephAlignment ""))
    ]

  showsPrec d h = (\s -> s)


data ElfSectionHeader = ElfSectionHeader
 { eshName      :: CUInt
 , eshType      :: CUInt
 , eshFlags     :: CULong
 , eshAddress   :: CULong
 , eshOffset    :: CULong
 , eshSize      :: CULong
 , eshLink      :: CUInt
 , eshInfo      :: CUInt
 , eshAlignment :: CULong
 , eshEntrySize :: CULong
 } deriving (Eq, Show)


instance Storable ElfSectionHeader where
  alignment _ = #{alignment ElfSectionHeader_t}
  sizeOf _    = #{size      ElfSectionHeader_t}
  peek p =
    ElfSectionHeader <$> #{peek ElfSectionHeader_t, sh_name      } p
                     <*> #{peek ElfSectionHeader_t, sh_type      } p
                     <*> #{peek ElfSectionHeader_t, sh_flags     } p
                     <*> #{peek ElfSectionHeader_t, sh_addr      } p
                     <*> #{peek ElfSectionHeader_t, sh_offset    } p
                     <*> #{peek ElfSectionHeader_t, sh_size      } p
                     <*> #{peek ElfSectionHeader_t, sh_link      } p
                     <*> #{peek ElfSectionHeader_t, sh_info      } p
                     <*> #{peek ElfSectionHeader_t, sh_addralign } p
                     <*> #{peek ElfSectionHeader_t, sh_entsize   } p

  poke p ElfSectionHeader{..} = do
                     #{poke ElfSectionHeader_t, sh_name      } p eshName
                     #{poke ElfSectionHeader_t, sh_type      } p eshType
                     #{poke ElfSectionHeader_t, sh_flags     } p eshFlags
                     #{poke ElfSectionHeader_t, sh_addr      } p eshAddress
                     #{poke ElfSectionHeader_t, sh_offset    } p eshOffset
                     #{poke ElfSectionHeader_t, sh_size      } p eshSize
                     #{poke ElfSectionHeader_t, sh_link      } p eshLink
                     #{poke ElfSectionHeader_t, sh_info      } p eshInfo
                     #{poke ElfSectionHeader_t, sh_addralign } p eshAlignment
                     #{poke ElfSectionHeader_t, sh_entsize   } p eshEntrySize
