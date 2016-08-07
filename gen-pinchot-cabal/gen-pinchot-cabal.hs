module Main (main) where

import qualified Cartel

version :: [Word]
version = [0,20,0,0]

properties :: Cartel.Properties
properties = Cartel.Properties
  { Cartel.name = "pinchot"
  , Cartel.version = version
  , Cartel.cabalVersion = Just (1,10)
  , Cartel.buildType = Just Cartel.simple
  , Cartel.license = Just Cartel.bsd3
  , Cartel.licenseFile = "LICENSE"
  , Cartel.licenseFiles = []
  , Cartel.copyright = "Copyright (c) 2015 - 2016 Omari Norman"
  , Cartel.author = "Omari Norman"
  , Cartel.maintainer = "omari@smileystation.com"
  , Cartel.stability = "Experimental"
  , Cartel.homepage = "http://www.github.com/massysett/pinchot"
  , Cartel.bugReports = "http://www.github.com/massysett/pinchot/issues"
  , Cartel.packageUrl = ""
  , Cartel.synopsis = "Write grammars, not parsers"
  , Cartel.description = ["Please see README.md"]
  , Cartel.category = "Development"
  , Cartel.testedWith = []
  , Cartel.dataFiles = []
  , Cartel.dataDir = ""
  , Cartel.extraSourceFiles = ["README.md"]
  , Cartel.extraDocFiles = []
  , Cartel.extraTmpFiles = []
  }

ghcOptions :: Cartel.HasBuildInfo a => a
ghcOptions = Cartel.ghcOptions
  [ "-W"
  ]

commonOptions :: Cartel.HasBuildInfo a => [a]
commonOptions
  = ghcOptions
  : Cartel.haskell2010
  : Cartel.hsSourceDirs ["lib"]
  : Cartel.otherExtensions ["TemplateHaskell"]
  : []

libraryDepends :: [Cartel.Package]
libraryDepends =
  [ Cartel.closedOpen "base" [4,9] [5]
  , Cartel.atLeast "containers" [0,5,6,2]
  , Cartel.atLeast "transformers" [0,4,2,0]
  , Cartel.atLeast "template-haskell" [2,11]
  , Cartel.atLeast "Earley" [0,11,0,1]
  , Cartel.atLeast "pretty-show" [1,6,9]
  , Cartel.atLeast "lens" [4,13]
  , Cartel.atLeast "ListLike" [4,2,1]
  , Cartel.atLeast "semigroups" [0,18,1]
  , Cartel.atLeast "non-empty-sequence" [0,2]
  ]

library
  :: [Cartel.NonEmptyString]
  -- ^ List of library modules
  -> [Cartel.LibraryField]
library libModules
  = Cartel.buildDepends libraryDepends
  : Cartel.exposedModules libModules
  : commonOptions

github :: Cartel.Section
github = Cartel.githubHead "massysett" "pinchot"

executable
  :: Cartel.FlagName
  -- ^ executables flag
  -> [Cartel.NonEmptyString]
  -- ^ Library modules
  -> String
  -- ^ Name of program
  -> Cartel.Section
executable flag mods progName = Cartel.executable progName
  [ block
  , Cartel.mainIs (progName ++ ".hs")
  ]
  where
    block = Cartel.condBlock (Cartel.invert $ Cartel.flag flag)
      (Cartel.buildable False, []) fields
      where
        fields =
          [ Cartel.buildable True
          , Cartel.hsSourceDirs ["exe"]
          , Cartel.buildDepends libraryDepends
          , Cartel.otherModules mods
          ] ++ commonOptions

sections
  :: Cartel.FlagName
  -- ^ executables flag
  -> [Cartel.NonEmptyString]
  -- ^ Library modules
  -> [Cartel.Section]
sections exeFlag libMods =
  [ github
  , executable exeFlag libMods "newman"
  , executable exeFlag libMods "newmanPretty"
  ]

main :: IO ()
main = Cartel.defaultMain $ do
  libModules <- Cartel.modules "../pinchot/lib"
  buildExe <- Cartel.makeFlag "executables" $ Cartel.FlagOpts
    { Cartel.flagDescription = "Build executables"
    , Cartel.flagDefault = False
    , Cartel.flagManual = True
    }
  return (properties, library libModules, sections buildExe libModules)
