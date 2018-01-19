{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}


module ElfTypes
(
  ElfHeader(..)
)
where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Data.Array (Array, listArray, elems)
import Lib (create_magic)


#include "ElfStructs64.h"


data ElfHeader = ElfHeader
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


instance Storable ElfHeader where
  alignment _ = #{alignment ElfHeader_t}
  sizeOf _    = #{size      ElfHeader_t}
  peek p =
    ElfHeader <$> (listArray (0,15) <$> (peekArray 16 (castPtr p) :: IO [CUChar]))
              <*> #{peek ElfHeader_t, e_type} p
              <*> #{peek ElfHeader_t, e_machine} p
              <*> #{peek ElfHeader_t, e_version} p
              <*> #{peek ElfHeader_t, e_entry} p
              <*> #{peek ElfHeader_t, e_phoff} p
              <*> #{peek ElfHeader_t, e_shoff} p
              <*> #{peek ElfHeader_t, e_flags} p
              <*> #{peek ElfHeader_t, e_ehsize} p
              <*> #{peek ElfHeader_t, e_phentsize} p
              <*> #{peek ElfHeader_t, e_phnum} p
              <*> #{peek ElfHeader_t, e_shentsize} p
              <*> #{peek ElfHeader_t, e_shnum} p
              <*> #{peek ElfHeader_t, e_shstrndx} p
  poke p ElfHeader{..} = do
    (pokeArray (castPtr p) (elems ehIdentification))
    #{poke ElfHeader_t, e_type} p ehObjectType
    #{poke ElfHeader_t, e_machine} p ehMachineType
    #{poke ElfHeader_t, e_version} p ehVersion
    #{poke ElfHeader_t, e_entry} p ehEntryPoint
    #{poke ElfHeader_t, e_phoff} p ehProgramHeaderOffset
    #{poke ElfHeader_t, e_shoff} p ehSectionHeaderOffset
    #{poke ElfHeader_t, e_flags} p ehFlags
    #{poke ElfHeader_t, e_ehsize} p ehHeaderSize
    #{poke ElfHeader_t, e_phentsize} p ehProgramHeaderEntrySize
    #{poke ElfHeader_t, e_phnum} p ehProgramHeaderNumEntries
    #{poke ElfHeader_t, e_shentsize} p ehSectionHeaderEntrySize
    #{poke ElfHeader_t, e_shnum} p ehSectionHeaderNumEntries
    #{poke ElfHeader_t, e_shstrndx} p ehSectionHeaderStringIndex

instance Show ElfHeader where
  show ElfHeader{..} = unlines [
      "ELF Header:"
    , "  Magic:   " ++ create_magic ehIdentification
    , "  Class:                             " ++ show(ehObjectType              )
    , "  Data:                              " ++ show()
    , "  Version:                           " ++ show(ehVersion                 )
    , "  OS/ABI:                            " ++ show()
    , "  ABI Version:                       " ++ show()
    , "  Type:                              " ++ show()
    , "  Machine:                           " ++ show(ehMachineType             )
    , "  Version:                           " ++ show()
    , "  Entry point address:               " ++ show(ehEntryPoint              )
    , "  Start of program headers:          " ++ show(ehProgramHeaderOffset     )
    , "  Start of section headers:          " ++ show(ehSectionHeaderOffset     )
    , "  Flags:                             " ++ show(ehFlags                   )
    , "  Size of this header:               " ++ show(ehHeaderSize              )
    , "  Size of program headers:           " ++ show(ehProgramHeaderEntrySize  )
    , "  Number of program headers:         " ++ show(ehProgramHeaderNumEntries )
    , "  Size of section headers:           " ++ show(ehSectionHeaderEntrySize  )
    , "  Number of section headers:         " ++ show(ehSectionHeaderNumEntries )
    , "  Section header string table index: " ++ show(ehSectionHeaderStringIndex)
    ]

  showsPrec d h = (\s -> s)
