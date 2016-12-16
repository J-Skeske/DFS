{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_use_haskell (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jackskehan/coding/theONE/use-haskell/.stack-work/install/x86_64-osx/nightly-2016-12-10/8.0.1.20161117/bin"
libdir     = "/Users/jackskehan/coding/theONE/use-haskell/.stack-work/install/x86_64-osx/nightly-2016-12-10/8.0.1.20161117/lib/x86_64-osx-ghc-8.0.1.20161117/use-haskell-0.1.0.0-F7dNwkeuN8BHaJ7CGjyfo9"
dynlibdir  = "/Users/jackskehan/coding/theONE/use-haskell/.stack-work/install/x86_64-osx/nightly-2016-12-10/8.0.1.20161117/lib/x86_64-osx-ghc-8.0.1.20161117"
datadir    = "/Users/jackskehan/coding/theONE/use-haskell/.stack-work/install/x86_64-osx/nightly-2016-12-10/8.0.1.20161117/share/x86_64-osx-ghc-8.0.1.20161117/use-haskell-0.1.0.0"
libexecdir = "/Users/jackskehan/coding/theONE/use-haskell/.stack-work/install/x86_64-osx/nightly-2016-12-10/8.0.1.20161117/libexec"
sysconfdir = "/Users/jackskehan/coding/theONE/use-haskell/.stack-work/install/x86_64-osx/nightly-2016-12-10/8.0.1.20161117/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "use_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "use_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "use_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "use_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "use_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "use_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
