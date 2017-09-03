module Paths_robotone (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/x86_64-linux-ghc-7.8.4/robotone-0.0.1"
datadir    = "/usr/local/share/x86_64-linux-ghc-7.8.4/robotone-0.0.1"
libexecdir = "/usr/local/libexec"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "robotone_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "robotone_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "robotone_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "robotone_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "robotone_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
