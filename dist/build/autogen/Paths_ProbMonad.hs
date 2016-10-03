module Paths_ProbMonad (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/h/ywang30/.cabal/bin"
libdir     = "/h/ywang30/.cabal/lib/x86_64-linux-ghc-7.10.2/ProbMonad-0.1.0.0-9udjbGwe93J9xTz4kziwhg"
datadir    = "/h/ywang30/.cabal/share/x86_64-linux-ghc-7.10.2/ProbMonad-0.1.0.0"
libexecdir = "/h/ywang30/.cabal/libexec"
sysconfdir = "/h/ywang30/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProbMonad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProbMonad_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ProbMonad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProbMonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProbMonad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
