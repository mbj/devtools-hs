module Devtools.Brittany
  ( Brittany.Config
  , testTree
  )
where

import           Control.Monad.Trans.Maybe      ( runMaybeT )
import           Data.Coerce
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Semigroup
import           Devtools.Prelude
import           Language.Haskell.Brittany      ( BrittanyError(..) )
import           Test.Tasty.MGolden             ( goldenTest )

import qualified Data.List                     as List
import qualified Data.Text.IO                  as Text
import qualified Language.Haskell.Brittany     as Brittany
import qualified System.Path                   as Path
import qualified System.Path.Directory         as Path
import qualified Test.Tasty                    as Tasty

testTree
  :: Maybe Path.RelFile -> [Path.RelFile] -> [Path.RelFile] -> IO Tasty.TestTree
testTree userConfigFile files filesToIgnore = do
  selectedConfig <- maybe (pure defaultConfig) getUserConfig userConfigFile

  pure $ mkTestTree selectedConfig (files List.\\ filesToIgnore)

mkTestTree :: Brittany.Config -> [Path.RelFile] -> Tasty.TestTree
mkTestTree config = Tasty.testGroup "Brittany" . fmap singleTest
 where
  singleTest :: Path.RelFile -> Tasty.TestTree
  singleTest (Path.toString -> filePath) = goldenTest filePath filePath $ do
    content <- Text.readFile filePath

    either (fail . show . fmap showBrittanyError) pure
      =<< Brittany.parsePrintModule config content

showBrittanyError :: BrittanyError -> String
showBrittanyError = \case
  ErrorInput         str -> str
  ErrorUnusedComment str -> str
  LayoutWarning      str -> str
  ErrorUnknownNode str _ -> str
  ErrorMacroConfig str _ -> "When parsing inline config: " <> str
  ErrorOutputCheck       -> "Output is not syntactically valid."

defaultConfig :: Brittany.Config
defaultConfig = Brittany.staticDefaultConfig
  { Brittany._conf_forward       = Brittany.ForwardOptions syntaxExtensions
  , Brittany._conf_errorHandling = debugConfig
  }
 where
  debugConfig :: Brittany.CErrorHandlingConfig Identity
  debugConfig = (Brittany._conf_errorHandling Brittany.staticDefaultConfig)
    { Brittany._econf_produceOutputOnErrors = coerce True
    }

  additionalExtensions :: [String]
  additionalExtensions =
    ["-XExplicitNamespaces", "-XPatternSynonyms", "-XNumericUnderscores"]

  syntaxExtensions :: Identity [String]
  syntaxExtensions = (additionalExtensions <>)
    <$> Brittany._options_ghc Brittany.forwardOptionsSyntaxExtsEnabled

getUserConfig :: Path.RelFile -> IO Brittany.Config
getUserConfig path@(Path.toString -> filePath) = do
  unlessM (Path.doesFileExist path) $ fail "Provided config file does not exist"

  mConfig <- runMaybeT $ Brittany.readConfigs mempty [filePath]

  let errorMsg =
        "Failed to obtain Brittany Config from provided file: " <> filePath
  maybe (fail errorMsg) pure mConfig
