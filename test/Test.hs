import           Devtools.Files
import           Devtools.Prelude
import           Test.Tasty
import           Test.Tasty.MGolden

import qualified Data.List                     as List
import qualified Data.Text                     as Text
import qualified Devtools
import qualified System.Path                   as Path

main :: IO ()
main = do
  files <- List.sort <$> findHaskellFiles [Path.relDir "src"]

  let filesListText = Text.pack . show $ Path.toString <$> files

  let filesGoldenTest = goldenTest "Get Haskell files in src" "test/files.txt"
        $ pure filesListText

  devtoolsTestTree <- Devtools.testTree Devtools.defaultConfig

  defaultMain $ testGroup "Devtools" [filesGoldenTest, devtoolsTestTree]
