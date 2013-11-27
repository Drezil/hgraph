module Paths_hgraph (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/thomas/.cabal/bin"
libdir     = "/home/thomas/.cabal/lib/hgraph-0.0.1/ghc-7.4.1"
datadir    = "/home/thomas/.cabal/share/hgraph-0.0.1"
libexecdir = "/home/thomas/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hgraph_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hgraph_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hgraph_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hgraph_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
