module Devtools.Files
  ( findHaskellFiles
  )
where

import Devtools.Prelude
import System.Path ((</>))

import qualified Data.List             as List
import qualified System.Path           as Path
import qualified System.Path.Directory as Path

findHaskellFiles :: [Path.RelDir] -> IO [Path.RelFile]
findHaskellFiles = fmap List.concat . traverse findFilesRecursive

findFilesRecursive :: Path.RelDir -> IO [Path.RelFile]
findFilesRecursive path = do
  filePath <- maybe (fail "File doesn't not exist") pure $ Path.fileFromDir path

  Path.doesFileExist filePath >>= \case
    True -> pure [filePath]
    _    -> Path.doesDirectoryExist path >>= \case
      True -> findFilesRecursive' path
        <&> List.filter ((== ".hs") . Path.takeExtension)
      False -> fail $ "Input folder does not exists: " <> Path.toString path
 where
  findFilesRecursive' :: Path.RelDir -> IO [Path.RelFile]
  findFilesRecursive' = listDirectoryFiles findFilesRecursive

  listDirectoryFiles
    :: (Path.RelDir -> IO [Path.RelFile]) -> Path.RelDir -> IO [Path.RelFile]
  listDirectoryFiles go topdir = Path.relDirectoryContents topdir >>= \case
    (relDirs, relfiles) ->
      (fmap (topdir </>) relfiles <>)
        .   List.concat
        <$> traverse getInnerFiles relDirs
   where
    getInnerFiles :: Path.RelDir -> IO [Path.RelFile]
    getInnerFiles innerDir = do
      let dir = topdir `Path.combine` innerDir

      Path.doesDirectoryExist dir >>= \case
        True  -> go dir
        False -> pure [Path.relFile $ Path.toString dir]
