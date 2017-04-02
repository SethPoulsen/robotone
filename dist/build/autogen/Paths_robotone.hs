module Paths_robotone (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/robotone-0.0.1/ghc-7.6.3"
datadir    = "/root/.cabal/share/robotone-0.0.1"
libexecdir = "/root/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "robotone_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "robotone_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "robotone_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "robotone_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
