#!/usr/bin/env stack
-- stack --resolver=lts-5.1 --install-ghc runghc --package=cartel

module Main where

import Cartel

pinchotVer :: [Word]
pinchotVer = [0,10,0,0]

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
earley = atleast "Earley" [0,10,1,0]

prettyShow :: Package
prettyShow = atleast "pretty-show" [1,6,9]

lens :: Package
lens = atleast "lens" [4,13]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions ["-Wall"]
  , otherExtensions ["TemplateHaskell"]
  , haskell2010
  , hsSourceDirs ["lib"]
  ]

libraryDepends :: [Package]
libraryDepends = [ base, containers, transformers, templateHaskell,
  earley, lens ]

props :: Properties
props = blank
  { name = "pinchot"
  , version = pinchotVer
  , cabalVersion = Just (1,14)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "2015 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/pinchot"
  , bugReports = "http://www.github.com/massysett/pinchot/issues"
  , synopsis = "Build parsers and ASTs for context-free grammars"
  , extraSourceFiles = ["README"]
  , description =
    [ "Pinchot provides a simple language that you use to write a Haskell"
    , "program that describes a context-free grammar.  When run, this program"
    , "creates a value representing the grammar.  Using this value, you can"
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
      , executable "print-postal-grammar" $
        [ mainIs "print-postal-grammar.hs"
        , condBlock (flag buildExe)
            (buildable True, ( [ otherModules libMods
                               , hsSourceDirs ["exe"]
                               , buildDepends libraryDepends
                               ] ++ commonOptions
                             )
            )
            [buildable False]
        ]

      , executable "postal-parser" $
        [ mainIs "postal-parser.hs"
        , condBlock (flag buildExe)
            (buildable True, ( [ otherModules libMods
                               , hsSourceDirs ["exe"]
                               , buildDepends (prettyShow : libraryDepends)
                               ] ++ commonOptions
                             )
            )
            [buildable False]
        ]

      , executable "parrot" $
        [ mainIs "parrot.hs"
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
