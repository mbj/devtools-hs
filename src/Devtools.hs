module Devtools
  ( Config(..)
  , FormatterOptions(..)
  , defaultConfig
  , defaultFomatterOptions
  , defaultMain
  , main
  , testTree
  )
where

import           Devtools.Files
import           Devtools.Prelude

import qualified Data.List.NonEmpty            as NE
import qualified Devtools.Brittany             as Brittany
import qualified Devtools.Dependencies         as Dependencies
import qualified Devtools.HLint                as HLint
import qualified System.Path                   as Path
import qualified Test.Tasty                    as Tasty

data Config = Config
  { hlintArguments   :: [String]
  , formatterOptions :: FormatterOptions
  }

data FormatterOptions = FormatterOptions
  { configFile    :: Maybe Path.RelFile
  , directories   :: NE.NonEmpty Path.RelDir
  , filesToIgnore :: [Path.RelFile]
  }

defaultFomatterOptions :: FormatterOptions
defaultFomatterOptions = FormatterOptions
  { configFile    = empty
  , directories   = Path.relDir "src" NE.:| [Path.relDir "test"]
  , filesToIgnore = []
  }

defaultConfig :: Config
defaultConfig =
  Config { hlintArguments = [], formatterOptions = defaultFomatterOptions }

defaultMain :: IO ()
defaultMain = main defaultConfig

main :: Config -> IO ()
main = Tasty.defaultMain <=< testTree

testTree :: Config -> IO Tasty.TestTree
testTree Config { formatterOptions = FormatterOptions {..}, ..} = do

  brittanyTestTree <-
    findHaskellFiles (NE.toList directories)
      >>= flip (Brittany.testTree configFile) filesToIgnore

  pure $ Tasty.testGroup
    "devtools"
    [Dependencies.testTree, HLint.testTree hlintArguments, brittanyTestTree]
