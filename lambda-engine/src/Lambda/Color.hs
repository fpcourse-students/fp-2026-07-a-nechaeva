-- | Terminal color. Auto mode follows the usual rules: color if
-- stdout is a TTY, unless @NO_COLOR@ is set or @TERM=dumb@.
module Lambda.Color
  ( ColorMode (..)
  , Palette (..)
  , parseColorMode
  , detectPalette
  , plainPalette
  , paint
  , withCode
  ) where

import Data.Char (toLower)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)

data ColorMode
  = ColorAuto
  | ColorAlways
  | ColorNever
  deriving (Eq, Show)

-- | ANSI prefixes. All empty when color is off, so callers can
-- concatenate blindly.
data Palette = Palette
  { palReset  :: String
  , palError  :: String
  , palOk     :: String
  , palFalse  :: String
  , palPrompt :: String
  , palBeta   :: String
  , palDelta  :: String
  , palMuted  :: String
  , palBinders :: [String]
  } deriving (Eq, Show)

parseColorMode :: String -> Maybe ColorMode
parseColorMode s = case map toLower s of
  "auto"    -> Just ColorAuto
  "always"  -> Just ColorAlways
  "never"   -> Just ColorNever
  "on"      -> Just ColorAlways
  "off"     -> Just ColorNever
  "yes"     -> Just ColorAlways
  "no"      -> Just ColorNever
  _         -> Nothing

plainPalette :: Palette
plainPalette = Palette
  { palReset  = ""
  , palError  = ""
  , palOk     = ""
  , palFalse  = ""
  , palPrompt = ""
  , palBeta   = ""
  , palDelta  = ""
  , palMuted  = ""
  , palBinders = replicate 5 ""
  }

ansiPalette :: Palette
ansiPalette = Palette
  { palReset  = "\ESC[0m"
  , palError  = "\ESC[1;31m"
  , palOk     = "\ESC[32m"
  , palFalse  = "\ESC[33m"
  , palPrompt = "\ESC[1;36m"
  , palBeta   = "\ESC[1;4;33m"
  , palDelta  = "\ESC[1;4;35m"
  , palMuted  = "\ESC[2m"
  , palBinders =
      [ "\ESC[1;33m"
      , "\ESC[1;32m"
      , "\ESC[1;34m"
      , "\ESC[4;36m"
      , "\ESC[1;35m"
      ]
  }

detectPalette :: ColorMode -> IO Palette
detectPalette mode = do
  tty <- hIsTerminalDevice stdout
  noColor <- lookupEnv "NO_COLOR"
  term <- lookupEnv "TERM"
  let disabled =
        maybe False (not . null) noColor
        || term == Just "dumb"
      want = case mode of
        ColorAlways -> True
        ColorNever  -> False
        ColorAuto   -> tty && not disabled
  return (if want then ansiPalette else plainPalette)

withCode :: String -> String -> String -> String
withCode code reset s
  | null code = s
  | otherwise = code ++ s ++ reset

-- | Wrap a substring in a color code. The span is byte/char offset
-- in the already pretty-printed term (we only print ASCII).
paint :: String -> String -> (Int, Int) -> String -> String
paint code reset (i, n) s
  | n <= 0 || null code = s
  | otherwise =
      let (pre, rest) = splitAt (max 0 i) s
          (mid, post) = splitAt n rest
      in  pre ++ code ++ mid ++ reset ++ post
