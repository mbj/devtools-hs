module Devtools (main, tests) where

import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Text (Text)
import System.IO (IO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as Text
import qualified System.Process.Typed as Process
import qualified Test.Tasty           as Tasty
import qualified Test.Tasty.MGolden   as Tasty

main :: IO ()
main = Tasty.defaultMain Devtools.tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "devtools" [stackDependencies]

stackDependencies :: Tasty.TestTree
stackDependencies
  = Tasty.goldenTest "test/stack-dependencies.txt" readDependenciesText
  where
    readDependenciesText :: IO Text
    readDependenciesText
      = Text.decodeUtf8 . LBS.toStrict <$> readDependencies

    readDependencies :: IO LBS.ByteString
    readDependencies
      = Process.readProcessStdout_
      $ Process.proc "stack" ["ls", "dependencies"]
