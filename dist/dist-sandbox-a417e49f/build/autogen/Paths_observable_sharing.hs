module Paths_observable_sharing (
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
version = Version [0,1,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mararon/chalmers/repos/signal/.cabal-sandbox/bin"
libdir     = "/home/mararon/chalmers/repos/signal/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.1/obser_CfnpHq0FW2OD0CJKAVrGwx"
datadir    = "/home/mararon/chalmers/repos/signal/.cabal-sandbox/share/x86_64-linux-ghc-7.10.1/observable-sharing-0.1.1.0"
libexecdir = "/home/mararon/chalmers/repos/signal/.cabal-sandbox/libexec"
sysconfdir = "/home/mararon/chalmers/repos/signal/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "observable_sharing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "observable_sharing_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "observable_sharing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "observable_sharing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "observable_sharing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
