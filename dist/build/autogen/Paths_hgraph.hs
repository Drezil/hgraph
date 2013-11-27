module Paths_hgraph (
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

bindir     = "/home/sdressel/.cabal/bin"
libdir     = "/home/sdressel/.cabal/lib/x86_64-linux-ghc-7.6.3/hgraph-0.0.1"
datadir    = "/home/sdressel/.cabal/share/x86_64-linux-ghc-7.6.3/hgraph-0.0.1"
libexecdir = "/home/sdressel/.cabal/libexec"
sysconfdir = "/home/sdressel/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hgraph_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hgraph_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hgraph_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hgraph_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hgraph_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
