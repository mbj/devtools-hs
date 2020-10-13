module Devtools
  ( Config(..)
  , Target(..)
  , defaultConfig
  , defaultMain
  , main
  , testTree
  )
where

import Devtools.Config
import Devtools.Prelude
import System.IO (putStrLn)

import qualified Devtools.Dependencies as Dependencies
import qualified Devtools.HLint        as HLint
import qualified Test.Tasty            as Tasty

defaultConfig :: Config
defaultConfig = Config
  { hlintArguments = []
  , targets        = []
  }

defaultMain :: IO ()
defaultMain = main defaultConfig

main :: Config -> IO ()
main config = do
  putStrLn empty
  Tasty.defaultMain $ testTree config

testTree :: Config -> Tasty.TestTree
testTree Config{..} = Tasty.testGroup "devtools"
  [ Dependencies.testTree targets
  , HLint.testTree hlintArguments
  ]
