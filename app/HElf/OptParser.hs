module HElf.OptParser where

import Options.Applicative
import Data.Semigroup ((<>))


data HElfOptions = Options
  { displayAll :: Bool
  , displayFileHeader :: Bool
  , displayProgramHeaders :: Bool
  , displaySectionHeaders :: Bool
  , displaySectionGroups :: Bool
  , displaySectionDetails :: Bool
  , displayHeaders :: Bool
  , displaySymbolTable :: Bool
  , displayDynamicSymbolTable :: Bool
  , displayCoreNotes :: Bool
  , displayRelocations :: Bool
  , displayUnwindInfo :: Bool
  , displayDynamicSection :: Bool
  , displayVersionSections :: Bool
  , displayHElfVersion :: Bool
  }
