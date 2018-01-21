module HElf.OptParser where

import Options.Applicative
import Data.Semigroup ((<>))


data HElfOptions = Options
  { filenames                 :: [String]
  , displayAll                :: Bool
  , displayFileHeader         :: Bool
  , displayProgramHeaders     :: Bool
  , displaySectionHeaders     :: Bool
  , displaySectionGroups      :: Bool
  , displaySectionDetails     :: Bool
  , displayHeaders            :: Bool
  , displaySymbolTable        :: Bool
  , displayDynamicSymbolTable :: Bool
  , displayCoreNotes          :: Bool
  , displayRelocations        :: Bool
  , displayUnwindInfo         :: Bool
  , displayDynamicSection     :: Bool
  , displayVersionSections    :: Bool
  , displayHElfVersion        :: Bool
  }


optparser :: Parser HElfOptions
optparser = Options
         <$> some (argument str (metavar "FILES..."))
         <*> switch (
                long "all" <>
                short 'a' <>
                help "Equivalent to: -h -l -S -s -r -d -V -A -I")
         <*> switch (
                long "file-header" <>
                short 'h' <>
                help "Display the ELF file header")
         <*> switch (
                long "program-headers" <>
                long "segments" <>
                short 'l' <>
                help "Display the program headers")
         <*> switch (
                long "section-headers" <>
                long "sections" <>
                short 'S' <>
                help "Display the section headers")
         <*> switch (
                long "section-groups" <>
                short 'g' <>
                help "Display the section groups")
         <*> switch (
                long "section-details" <>
                short 't' <>
                help "Display the section details")
         <*> switch (
                long "headers" <>
                short 'e' <>
                help "Equivalent to: -h -l -S")
         <*> switch (
                long "symbols" <>
                long "syms" <>
                short 's' <>
                help "Display the symbol table")
         <*> switch (
                long "dyn-syms" <>
                help "Display the dynamic symbol table")
         <*> switch (
                long "notes" <>
                short 'n' <>
                help "Display the core notes (if present)")
         <*> switch (
                long "relocs" <>
                short 'r' <>
                help "Display the relocations (if present)")
         <*> switch (
                long "unwind" <>
                short 'u' <>
                help "Display the unwind info (if present)")
         <*> switch (
                long "dynamic" <>
                short 'd' <>
                help "Display the dynamic section (if present)")
         <*> switch (
                long "version-info" <>
                short 'V' <>
                help "Display the version sections (if present)")
         <*> switch (
                long "version" <>
                short 'v' <>
                help "Display the version info of helf")
