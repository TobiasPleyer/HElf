module HElf.Show where


import Data.Array
import Foreign.C.Types (CUChar, CUShort, CUInt)
import Data.List (intercalate)
import Numeric (showHex)


showHexLeadingZeroes :: Int -> CUChar -> String
showHexLeadingZeroes len hex =
  let
    s = (showHex hex) ""
    l = length s
  in
    if l > len
    then
      s
    else
      (take (len-l) (repeat '0')) ++ s

create_magic :: (Array Int CUChar) -> String
create_magic arr = intercalate " " (map (showHexLeadingZeroes 2) (elems arr))

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

showProgramHeaderType :: CUInt -> String
showProgramHeaderType c = case c of
  0x00 -> "NULL"
  0x01 -> "LOAD"
  0x02 -> "DYNAMIC"
  0x03 -> "INTERP"
  0x04 -> "NOTE"
  0x05 -> "SHLIB"
  0x06 -> "PHDR"
  _    -> "UNKNOWN (" ++ show c ++ ")"
