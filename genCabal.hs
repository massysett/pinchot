#!/usr/bin/env stack
-- stack --resolver=lts-5.4 --install-ghc runghc --package=cartel

module Main where

import Cartel

pinchotVer :: [Word]
pinchotVer = [0,18,0,0]

atleast :: NonEmptyString -> Version -> Package
atleast n v = package n (gtEq v)

base :: Package
base = closedOpen "base" [4,8,0,0] [5]

containers :: Package
containers = atleast "containers" [0,5,6,2]

transformers :: Package
transformers = atleast "transformers" [0,4,2,0]

templateHaskell :: Package
templateHaskell = atleast "template-haskell" [2,10]

earley :: Package
earley = atleast "Earley" [0,11,0,1]

prettyShow :: Package
prettyShow = atleast "pretty-show" [1,6,9]

lens :: Package
lens = atleast "lens" [4,13]

listlike :: Package
listlike = atleast "ListLike" [4,2,1]

semigroups :: Package
semigroups = atleast "semigroups" [0,18,1]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ otherExtensions ["TemplateHaskell"]
  , haskell2010
  , hsSourceDirs ["lib"]
  ]

libraryDepends :: [Package]
libraryDepends = [ base, containers, transformers, templateHaskell,
  earley, lens, listlike, semigroups ]

props :: Properties
props = blank
  { name = "pinchot"
  , version = pinchotVer
  , cabalVersion = Just (1,14)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "2015-2016 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/pinchot"
  , bugReports = "http://www.github.com/massysett/pinchot/issues"
  , synopsis = "Write grammars, not parsers"
  , extraSourceFiles = ["README"]
  , description =
    [ "Pinchot provides a simple language that you use to write a Haskell"
    , "value that describes a context-free grammar.  Using this value, you can"
    , "automatically generate data types corresponding to the grammar,"
    , "as well as an Earley parser to parse strings in that grammar."
    , ""
    , "For more documentation, see the Haddocks for the main Pinchot module."
    ]
  , category = "Development"
  }

main :: IO ()
main = defaultMain $ do
  libMods <- modules "lib"
  buildExe <- makeFlag "executables" $ FlagOpts
    { flagDescription = "Build executables"
    , flagDefault = False
    , flagManual = True
    }
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ githubHead "massysett" "penny"
      , executable "newman" $
        [ mainIs "newman.hs"
        , condBlock (flag buildExe)
            (buildable True, ( [ otherModules libMods
                               , hsSourceDirs ["exe"]
                               , buildDepends libraryDepends
                               ] ++ commonOptions
                             )
            )
            [buildable False]
        ]
      ]
    )
