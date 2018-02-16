module HElf.Show where


import Data.Array
import Data.Bits
import Foreign.C.Types (CUChar, CUShort, CUInt, CULong)
import Data.List (intercalate)
import Numeric (showHex)


leftPad :: Int -> Char -> String -> String
leftPad len c str =
  let
    l = length str
  in
    if l > len
    then
      str
    else
      (take (len-l) (repeat c)) ++ str


rightPad :: Int -> Char -> String -> String
rightPad len c str =
  let
    l = length str
  in
    if l > len
    then
      str
    else
      str ++ (take (len-l) (repeat c))


showHexPadded :: (Integral a, Show a) => Int -> a -> String
showHexPadded w = (leftPad w '0') . ($ "") . showHex


create_magic :: (Array Int CUChar) -> String
create_magic arr = intercalate " " (map (showHexPadded 2) (elems arr))

showElfClass :: CUChar -> String
showElfClass c = case c of
  1 -> "ELF32"
  2 -> "ELF64"
  _ -> "unknown"

showElfData :: CUChar -> String
showElfData c = case c of
  1 -> "2's complement, little endian"
  2 -> "2's complement, big endian"
  _ -> "unknown"

showElfOSABI :: CUChar -> String
showElfOSABI c = case c of
  0x00 -> "System V"
  0x01 -> "HP-UX"
  0x02 -> "NetBSD"
  0x03 -> "Linux"
  0x04 -> "GNU Hurd"
  0x06 -> "Solaris"
  0x07 -> "AIX"
  0x08 -> "IRIX"
  0x09 -> "FreeBSD"
  0x0A -> "Tru64"
  0x0B -> "Novell Modesto"
  0x0C -> "OpenBSD"
  0x0D -> "OpenVMS"
  0x0E -> "NonStop Kernel"
  0x0F -> "AROS"
  0x10 -> "Fenix OS"
  0x11 -> "CloudABI"
  0x53 -> "Sortix"
  _ -> "unknown"

showElfType :: CUShort -> String
showElfType c = case c of
  1 -> "RELOC (Relocatable file)"
  2 -> "EXEC (Executable file)"
  3 -> "SHARED"
  4 -> "CORE"
  _ -> "unknown"

showElfMachineType :: CUShort -> String
showElfMachineType c = case c of
  0x00 -> "No specific instruction set"
  0x02 -> "SPARC"
  0x03 -> "x86"
  0x08 -> "MIPS"
  0x14 -> "PowerPC"
  0x16 -> "S390"
  0x28 -> "ARM"
  0x2A -> "SuperH"
  0x32 -> "IA-64"
  0x3E -> "x86-64"
  0xB7 -> "AArch64"
  0xF3 -> "RISC-V"
  _    -> "unknown"

showProgramHeaderType :: CUInt -> String
showProgramHeaderType c = case c of
  0x00 -> "NULL"
  0x01 -> "LOAD"
  0x02 -> "DYNAMIC"
  0x03 -> "INTERP"
  0x04 -> "NOTE"
  0x05 -> "SHLIB"
  0x06 -> "PHDR"
  _    -> "unknown"


showProgramHeaderFlags :: CUInt -> String
showProgramHeaderFlags i =
  ' ' : (if (i .&. 4) > 0 then 'R' else ' ')
      : (if (i .&. 2) > 0 then 'W' else ' ')
      : (if (i .&. 1) > 0 then 'E' else ' ')
      : "    "


showSectionType :: CUInt -> String
showSectionType i = case i of
  0x00 -> "NULL"
  0x01 -> "PROGBITS"
  0x02 -> "SYMTAB"
  0x03 -> "STRTAB"
  0x04 -> "RELA"
  0x05 -> "HASH"
  0x06 -> "DYNAMIC"
  0x07 -> "NOTE"
  0x08 -> "NOBITS"
  0x09 -> "REL"
  0x0A -> "SHLIB"
  0x0B -> "DYNSYM"
  0x0E -> "INIT_ARRAY"
  0x0F -> "FINI_ARRAY"
  0x10 -> "PREINIT_ARRAY"
  0x11 -> "GROUP"
  0x12 -> "SYMTAB_SHNDX"
  0x13 -> "NUM"
  _    -> "unknown"


showSectionFlags :: CULong -> String
showSectionFlags f =
  ' ' : filter (/= ' ') (
        (if (f .&. 0x001) > 0 then 'W' else ' ')
      : (if (f .&. 0x002) > 0 then 'A' else ' ')
      : (if (f .&. 0x004) > 0 then 'X' else ' ')
      : (if (f .&. 0x010) > 0 then 'M' else ' ')
      : (if (f .&. 0x020) > 0 then 'S' else ' ')
      : (if (f .&. 0x040) > 0 then 'I' else ' ')
      : (if (f .&. 0x080) > 0 then 'L' else ' ')
      : (if (f .&. 0x100) > 0 then 'O' else ' ')
      : (if (f .&. 0x200) > 0 then 'G' else ' ')
      : (if (f .&. 0x400) > 0 then 'T' else ' ')
      : (if (f .&. 0x0ff00000) > 0 then 'o' else ' ')
      : (if (f .&. 0xf0000000) > 0 then 'p' else ' ')
      : (if (f .&. 0x4000000) > 0 then 'R' else ' ')
      : (if (f .&. 0x8000000) > 0 then 'E' else ' ')
      : "")
