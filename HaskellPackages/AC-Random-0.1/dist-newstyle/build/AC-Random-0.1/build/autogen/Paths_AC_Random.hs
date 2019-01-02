{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_AC_Random (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/omykhron/.cabal/bin"
libdir     = "/home/omykhron/.cabal/lib/x86_64-linux-ghc-8.0.2/.fake.AC-Random-0.1"
dynlibdir  = "/home/omykhron/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/omykhron/.cabal/share/x86_64-linux-ghc-8.0.2/AC-Random-0.1"
libexecdir = "/home/omykhron/.cabal/libexec"
sysconfdir = "/home/omykhron/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AC_Random_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AC_Random_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AC_Random_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AC_Random_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AC_Random_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AC_Random_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
