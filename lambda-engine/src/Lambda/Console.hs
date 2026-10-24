{-# LANGUAGE CPP #-}

-- | Make the program safe to run in any terminal.
--
-- The messages use characters outside the legacy Windows code pages (‘ ’ λ ⌜ ⌝ α),
-- and GHC encodes output with the console code page, so in a stock PowerShell a
-- single such character used to abort the run with
-- @commitBuffer: invalid argument (cannot encode character '\\8216')@.
-- Files are a separate trap: @readFile@ decodes with the same code page, while
-- homework files are always UTF-8.
--
-- Used by the @lambda@ executable and by the test runner of every homework
-- (@test/Spec.hs@), Haskell ones included.
module Lambda.Console
  ( setupConsole
  ) where

import GHC.IO.Encoding (getLocaleEncoding, mkTextEncoding, setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stderr, stdout)

#if defined(mingw32_HOST_OS)
import Control.Exception (SomeException, try)
import System.Win32.Console (setConsoleOutputCP)
#endif

-- | Call first thing in @main@. Files are read and written as UTF-8. Output is
-- UTF-8 where the terminal takes it: always outside Windows, and on Windows
-- after switching the console to code page 65001. If the switch fails, output
-- keeps the console code page, and characters it lacks are replaced instead of
-- raising an exception.
setupConsole :: IO ()
setupConsole = do
  console <- getLocaleEncoding
  setLocaleEncoding utf8
  unicode <- switchToUtf8
  out <- if unicode then return utf8 else mkTextEncoding (show console ++ "//TRANSLIT")
  mapM_ (`hSetEncoding` out) [stdout, stderr]

switchToUtf8 :: IO Bool
#if defined(mingw32_HOST_OS)
switchToUtf8 = do
  r <- try (setConsoleOutputCP 65001) :: IO (Either SomeException ())
  return (either (const False) (const True) r)
#else
switchToUtf8 = return True
#endif
