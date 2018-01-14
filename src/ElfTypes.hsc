{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}


module ElfTypes
(
    ElfHeader(..),
)
where

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


#include "ElfStructs64.h"


data ElfHeader = ElfHeader
  { h_ident     :: String
  , h_type      :: Int
  , h_machine   :: Int
  , h_version   :: Int
  , h_entry     :: Int
  , h_phoff     :: Int
  , h_shoff     :: Int
  , h_flags     :: Int
  , h_ehsize    :: Int
  , h_phentsize :: Int
  , h_phnum     :: Int
  , h_shentsize :: Int
  , h_shnum     :: Int
  , h_shstrndx  :: Int
  } deriving (Eq, Show)


instance Storable ElfHeader where
  alignment _ = #{alignment ElfHeader_t}
  sizeOf _    = #{size      ElfHeader_t}
  peek p =
    ElfHeader <$> (peekCString (castPtr p))
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
    c_ident_ptr <- newCString h_ident
    c_ident_arr <- peekArray 16 c_ident_ptr
    pokeArray (castPtr p) c_ident_arr
    #{poke ElfHeader_t, e_type} p h_type
    #{poke ElfHeader_t, e_machine} p h_machine
    #{poke ElfHeader_t, e_version} p h_version
    #{poke ElfHeader_t, e_entry} p h_entry
    #{poke ElfHeader_t, e_phoff} p h_phoff
    #{poke ElfHeader_t, e_shoff} p h_shoff
    #{poke ElfHeader_t, e_flags} p h_flags
    #{poke ElfHeader_t, e_ehsize} p h_ehsize
    #{poke ElfHeader_t, e_phentsize} p h_phentsize
    #{poke ElfHeader_t, e_phnum} p h_phnum
    #{poke ElfHeader_t, e_shentsize} p h_shentsize
    #{poke ElfHeader_t, e_shnum} p h_shnum
    #{poke ElfHeader_t, e_shstrndx} p h_shstrndx
